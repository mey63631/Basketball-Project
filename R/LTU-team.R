# LTU women's basketball: season analysis
# Run from the project root with source("R/LTU-team.R").
# This prepares the objects used by prediction, per-game networks and league comparison.
# The main workflow still downloads the live feed, as in the original script.

# 1. Packages and settings ----

library(httr)
library(jsonlite)
library(stringr)
library(purrr)
library(tidyr)
library(glmnet)
library(igraph)
library(ggraph)
library(ggplot2)
library(scales)
library(grid)
library(dplyr)

PERIOD_LENGTH_SECONDS <- 600
SCORE_POINTS <- c("2pt" = 2, "3pt" = 3, "freeThrow" = 1)

ltu_settings <- list(
  team = "LTU",
  season = "2025",
  min_coverage = 0.50,
  min_pair_minutes = 15,
  min_pair_games = 2,
  lambda_choice = "lambda.1se"
)
# The regression functions retain their original 15-second stint threshold.
# The usage network uses its own threshold: one shared minute in one game.
ltu_paths <- list(
  tables = file.path("outputs", "tables"),
  figures = "Figures"
)
for (folder in ltu_paths) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
}

# Keep all CSV paths in one place.
write_ltu_table <- function(x, filename) {
  write.csv(x, file = file.path(ltu_paths$tables, filename), row.names = FALSE)
}

# 2. Functions for downloading play-by-play ----

# Convert the source clock, such as PT9M45S, to seconds remaining.
parse_clock <- function(x) {
  mins <- as.numeric(sub("PT(\\d+)M.*", "\\1", x))
  secs <- as.numeric(sub(".*M(\\d+(?:\\.\\d+)?)S", "\\1", x))
  mins * 60 + secs
}

# Read either event structure returned by the feed.
extract_events <- function(d) {
  pd <- d$data$periodData

  # New response structure
  if (!is.null(pd) && "events" %in% names(pd)) {
    events <- bind_rows(pd$events, .id = "quarter")
    if (nrow(events) > 0) return(events)
  }

  # Old response structure
  if (!is.null(d$data$pbp)) {
    events <- d$data$pbp |>
      lapply(function(q) q$events) |>
      bind_rows(.id = "quarter")

    if (nrow(events) > 0) return(events)
  }

  NULL
}

# Download one fixture and return its event table. 
get_match_pbp <- function(fixture_state, base = "https://embed-api.eui.connect.sportradar.com/v1/embed/303") {

  url1 <- paste0(base, "/fixture_detail?state=", fixture_state)
  res1 <- tryCatch(GET(url1, add_headers("User-Agent" = "Mozilla/5.0")), error = function(e) NULL)
  if (is.null(res1) || status_code(res1) != 200) {
    warning("Step 1 failed for: ", fixture_state)
    return(NULL)
  }
  data1 <- fromJSON(content(res1, "text", encoding = "UTF-8"), flatten = TRUE)

  pbp_tab <- data1$data$tabs[data1$data$tabs$label == "Play By Play", ]
  if (nrow(pbp_tab) == 0) {
    warning("No Play By Play tab found for: ", fixture_state)
    return(NULL)
  }
  pbp_state <- str_extract(pbp_tab$link, "(?<=~w=f~).+")

  url2 <- paste0(base, "/fixture_detail?state=", pbp_state)
  res2 <- tryCatch(GET(url2, add_headers("User-Agent" = "Mozilla/5.0")), error = function(e) NULL)
  if (is.null(res2) || status_code(res2) != 200) {
    warning("Step 2 failed for: ", fixture_state)
    return(NULL)
  }
  data <- fromJSON(content(res2, "text", encoding = "UTF-8"), flatten = TRUE)

  all_events <- extract_events(data)

  if (is.null(all_events) || nrow(all_events) == 0) {
    warning("No events found for: ", fixture_state)
    return(NULL)
  }

  team_lookup <- data.frame(
    entityId = data$data$fixture$competitors$entityId,
    team     = data$data$fixture$competitors$name
  )

  all_events %>%
    left_join(team_lookup, by = "entityId") %>%
    mutate(
      clock_seconds = parse_clock(clock),
      period        = periodId,
      player        = name,
      action        = eventType,
      detail        = eventSubType,
      fixtureId     = data$data$fixture$fixtureId,
      matchup       = paste(team_lookup$team, collapse = " vs ")
    ) %>%
    select(fixtureId, matchup, period, clock_seconds, team, player, action, detail,
           success, x, y, bib, personId, eventId)
}

# 3. Functions for player lookup and lineups ----

# Keep player IDs for calculations and join names for readable outputs.
build_player_lookup <- function(pbp_df) {
  pbp_df %>%
    filter(!is.na(personId), !is.na(player)) %>%
    mutate(personId = as.character(personId)) %>%
    group_by(personId) %>%
    summarise(player_name = first(player), .groups = "drop")
}

# Infer starters from activity before the first substitution.
get_starting_lineup <- function(period_df, team_name) {
  team_events <- period_df %>% filter(team == team_name)
  first_sub_row <- team_events %>%
    mutate(row_num = row_number()) %>%
    filter(action == "substitution") %>%
    slice(1)
  if (nrow(first_sub_row) > 0) {
    if (first_sub_row$row_num > 1) {
      pre_sub <- team_events %>% slice(seq_len(first_sub_row$row_num - 1))
    } else {
      pre_sub <- team_events %>% slice(0)
    }
  } else {
    pre_sub <- team_events
  }
  starters <- unique(as.character(pre_sub$personId))
  starters <- starters[!is.na(starters)]
  starters <- head(starters, 5)
  if (length(starters) != 5) {
    cat("WARNING: found", length(starters), "pre-sub starters for",
        team_name, "in this period (expected 5)\n")
  }
  starters
}

# Quarter-break lineups are carried over, which can miss unlogged changes.
# This check flags inconsistent activity; it does not correct the lineup.
validate_period_start <- function(period_df, team_name, carried_lineup, fid, per) {
  team_events <- period_df %>% filter(team == team_name)
  first_sub_row <- team_events %>%
    mutate(row_num = row_number()) %>%
    filter(action == "substitution") %>%
    slice(1)
  pre_sub <- if (nrow(first_sub_row) > 0) {
    if (first_sub_row$row_num > 1) {
      team_events %>% slice(seq_len(first_sub_row$row_num - 1))
    } else {
      team_events %>% slice(0)
    }
  } else {
    team_events
  }
  actors <- unique(as.character(pre_sub$personId))
  actors <- actors[!is.na(actors)]
  unexpected <- setdiff(actors, carried_lineup)
  if (length(unexpected) > 0) {
    cat("QUARTER-BREAK FLAG: fixture", fid, "period", per, "-", team_name,
        "- personId(s)", paste(unexpected, collapse = ", "),
        "acted before any substitution this period but were NOT in the carried-over lineup.",
        "Possible un-logged lineup change.\n")
  }
  length(unexpected)
}

# Track substitutions and scoring within each constant-lineup interval.
build_stints <- function(
    pbp_df,
    period_length = PERIOD_LENGTH_SECONDS,
    half_boundary_periods = c()
) {
  all_stints <- list()
  n_quarter_flags <- 0

  pbp_df <- pbp_df %>%
    mutate(.orig_order = row_number())

  fixture_ids <- unique(pbp_df$fixtureId)

  for (fid in fixture_ids) {
    game_df <- pbp_df %>%
      filter(fixtureId == fid) %>%
      arrange(period, desc(clock_seconds), .orig_order) %>%
      mutate(is_sub = action == "substitution")

    teams <- unique(na.omit(game_df$team))

    if (length(teams) != 2) {
      next
    }

    team_a <- teams[1]
    team_b <- teams[2]

    first_period <- min(game_df$period)
    period1_df <- game_df %>%
      filter(period == first_period)

    lineup <- list()
    lineup[[team_a]] <- get_starting_lineup(
      period1_df,
      team_a
    )
    lineup[[team_b]] <- get_starting_lineup(
      period1_df,
      team_b
    )

    # Points scored during the current stint.
    points <- setNames(c(0, 0), c(team_a, team_b))

    # Persistent game score. Unlike `points`, this is never reset.
    cumulative_points <- setNames(
      c(0, 0),
      c(team_a, team_b)
    )

    # Score at the exact moment the current stint began.
    stint_start_score <- cumulative_points

    current_period <- first_period
    stint_start_clock <- period_length
    n <- nrow(game_df)

    flush <- function(end_clock, per) {
      team_pairs <- list(
        c(team_a, team_b),
        c(team_b, team_a)
      )

      for (pair in team_pairs) {
        t <- pair[1]
        opp <- pair[2]

        if (length(lineup[[t]]) == 5) {
          own_roster <- sort(lineup[[t]])
          opp_roster <- sort(lineup[[opp]])

          all_stints[[length(all_stints) + 1]] <<- tibble(
            fixtureId = fid,
            period = per,
            team = t,
            lineup = list(own_roster),
            opponent = opp,
            opp_lineup = list(opp_roster),
            team_points = points[[t]],
            opp_points = points[[opp]],
            duration_sec = stint_start_clock - end_clock,
            start_clock_seconds = stint_start_clock,
            end_clock_seconds = end_clock,
            score_margin_start =
              stint_start_score[[t]] -
              stint_start_score[[opp]]
          )
        }
      }

      # Roll this stint's scoring into the persistent game score.
      cumulative_points <<- cumulative_points + points
      stint_start_score <<- cumulative_points
    }

    i <- 1

    while (i <= n) {
      row <- game_df[i, ]

      if (row$period != current_period) {
        flush(0, current_period)

        points <- setNames(c(0, 0), c(team_a, team_b))
        current_period <- row$period
        stint_start_clock <- period_length

        new_period_df <- game_df %>%
          filter(period == current_period)

        if (current_period %in% half_boundary_periods) {
          lineup[[team_a]] <- get_starting_lineup(
            new_period_df,
            team_a
          )
          lineup[[team_b]] <- get_starting_lineup(
            new_period_df,
            team_b
          )
        } else {
          n_quarter_flags <- n_quarter_flags +
            validate_period_start(
              new_period_df,
              team_a,
              lineup[[team_a]],
              fid,
              current_period
            ) +
            validate_period_start(
              new_period_df,
              team_b,
              lineup[[team_b]],
              fid,
              current_period
            )
        }
      }

      if (isTRUE(row$is_sub)) {
        same_time_idx <- i
        j <- i

        while (j + 1 <= n &&
               game_df$period[j + 1] == current_period &&
               isTRUE(game_df$is_sub[j + 1]) &&
               game_df$clock_seconds[j + 1] == row$clock_seconds) {
          j <- j + 1
          same_time_idx <- c(same_time_idx, j)
        }

        flush(row$clock_seconds, current_period)

        for (k in same_time_idx) {
          r <- game_df[k, ]
          tm <- r$team
          pid <- as.character(r$personId)

          if (r$detail == "out") {
            lineup[[tm]] <- setdiff(
              lineup[[tm]],
              pid
            )
          } else if (r$detail == "in") {
            lineup[[tm]] <- union(
              lineup[[tm]],
              pid
            )
          }
        }

        points <- setNames(c(0, 0), c(team_a, team_b))
        stint_start_clock <- row$clock_seconds
        i <- j + 1

        next
      }

      if (
        row$action %in% names(SCORE_POINTS) &&
        isTRUE(row$success)
      ) {
        points[[row$team]] <-
          points[[row$team]] +
          SCORE_POINTS[[row$action]]
      }

      i <- i + 1
    }

    flush(0, current_period)
  }

  cat(
    "\nTotal quarter-break lineup discrepancy flags across dataset:",
    n_quarter_flags,
    "- document this count as a data limitation in your write-up.\n"
  )

  bind_rows(all_stints)
}

# 4. Functions for coverage and quality checks ----

# Compare reconstructed minutes with a 40-minute regulation game.
compute_game_coverage <- function(stints_df, team_name, expected_periods = 4, period_length = 600) {
  stints_df %>%
    filter(team == team_name) %>%
    group_by(fixtureId) %>%
    summarise(total_sec = sum(duration_sec), .groups = "drop") %>%
    mutate(pct_of_full_game = round(total_sec / (expected_periods * period_length), 2)) %>%
    arrange(pct_of_full_game)
}

# Remove games below the coverage threshold.
exclude_broken_games <- function(stints_df, team_name, min_pct = 0.5) {
  coverage <- compute_game_coverage(stints_df, team_name)
  bad_fixtures <- coverage %>% filter(pct_of_full_game < min_pct) %>% pull(fixtureId)

  if (length(bad_fixtures) > 0) {
    cat("Excluding", length(bad_fixtures),
        "game(s) with incomplete lineup-tracking coverage (<", min_pct * 100,
        "% of expected game time) - starting lineup detection likely failed:\n")
    print(coverage %>% filter(fixtureId %in% bad_fixtures))
  } else {
    cat("No games excluded - all games retained at least", min_pct * 100, "% coverage.\n")
  }

  stints_df %>% filter(!fixtureId %in% bad_fixtures)
}

# Compare stint scores with scoring events from the same feed.
validate_reconstruction <- function(stints_df, pbp_df, team_name,
                                    expected_periods = 4,
                                    period_length = 600) {
  retained_games <- stints_df %>%
    filter(team == team_name) %>%
    distinct(fixtureId)

  reconstructed <- stints_df %>%
    filter(team == team_name) %>%
    group_by(fixtureId) %>%
    summarise(
      reconstructed_team_points = sum(team_points),
      reconstructed_opp_points = sum(opp_points),
      reconstructed_minutes = sum(duration_sec) / 60,
      negative_duration = sum(duration_sec < 0),
      zero_duration = sum(duration_sec == 0),
      incomplete_lineup = sum(map_int(lineup, length) != 5),
      .groups = "drop"
    )

  pbp_scores <- pbp_df %>%
    semi_join(retained_games, by = "fixtureId") %>%
    filter(
      action %in% names(SCORE_POINTS),
      !is.na(success), success
    ) %>%
    mutate(event_points = unname(SCORE_POINTS[action])) %>%
    group_by(fixtureId) %>%
    summarise(
      pbp_team_points = sum(event_points[team == team_name], na.rm = TRUE),
      pbp_opp_points = sum(event_points[team != team_name], na.rm = TRUE),
      .groups = "drop"
    )

  reconstructed %>%
    left_join(pbp_scores, by = "fixtureId") %>%
    mutate(
      expected_minutes = expected_periods * period_length / 60,
      coverage = reconstructed_minutes / expected_minutes,
      team_score_difference = reconstructed_team_points - pbp_team_points,
      opp_score_difference = reconstructed_opp_points - pbp_opp_points,
      score_check = if_else(
        team_score_difference == 0 & opp_score_difference == 0,
        "Match", "Investigate"
      )
    ) %>%
    arrange(coverage)
}

# 5. Functions for player ratings and pair scores ----

# Summarise raw pair performance as a descriptive comparison.
naive_pair_synergy <- function(stints_df, team_name, player_lookup,
                               min_duration_sec = 15) {
  team_stints <- stints_df %>%
    filter(team == team_name, duration_sec >= min_duration_sec)
  pair_rows <- team_stints %>%
    mutate(stint_id = row_number()) %>%
    mutate(pairs = map(lineup, ~ combn(.x, 2, simplify = FALSE))) %>%
    select(stint_id, fixtureId, pairs, team_points, opp_points, duration_sec) %>%
    unnest(pairs) %>%
    mutate(id_1 = map_chr(pairs, 1), id_2 = map_chr(pairs, 2)) %>%
    select(-pairs)

  pair_rows %>%
    group_by(id_1, id_2) %>%
    summarise(
      minutes_together = sum(duration_sec) / 60,
      pts_for = sum(team_points),
      pts_against = sum(opp_points),
      n_games_together = n_distinct(fixtureId),
      .groups = "drop"
    ) %>%
    filter(minutes_together > 0) %>%
    mutate(
      net_points = pts_for - pts_against,
      net_per_minute = round(net_points / minutes_together, 2),
      minutes_together = round(minutes_together, 1)
    ) %>%
    left_join(player_lookup, by = c("id_1" = "personId")) %>%
    rename(player_1 = player_name) %>%
    left_join(player_lookup, by = c("id_2" = "personId")) %>%
    rename(player_2 = player_name) %>%
    select(player_1, player_2, id_1, id_2, minutes_together, n_games_together,
           net_points, net_per_minute) %>%
    arrange(desc(net_per_minute))
}

# Build player and opponent-team indicators; weight stints by duration.
build_player_design_matrix <- function(
    stints_df,
    team_name,
    min_duration_sec = 15
) {

  team_stints <- stints_df %>%
    filter(
      team == team_name,
      duration_sec >= min_duration_sec
    ) %>%
    mutate(
      lineup = map(
        lineup,
        ~ as.character(.x[
          !is.na(.x) & nzchar(as.character(.x))
        ])
      ),
      stint_id = row_number()
    )

  # Collect all valid player IDs
  all_players <- team_stints$lineup %>%
    unlist(use.names = FALSE) %>%
    as.character()

  all_players <- sort(unique(
    all_players[
      !is.na(all_players) &
        nzchar(all_players)
    ]
  ))

  player_matrix <- matrix(
    0,
    nrow = nrow(team_stints),
    ncol = length(all_players),
    dimnames = list(NULL, all_players)
  )

  for (i in seq_len(nrow(team_stints))) {

    players_on <- as.character(
      team_stints$lineup[[i]]
    )

    players_on <- players_on[
      !is.na(players_on) &
        nzchar(players_on)
    ]

    # Match player IDs to matrix-column positions
    player_positions <- match(
      players_on,
      all_players
    )

    if (anyNA(player_positions)) {
      warning(
        "Unmatched player ID in stint ",
        i,
        ": ",
        paste(
          players_on[is.na(player_positions)],
          collapse = ", "
        )
      )

      player_positions <- player_positions[
        !is.na(player_positions)
      ]
    }

    if (length(player_positions) > 0) {
      player_matrix[i, player_positions] <- 1
    }
  }

  # Opponent-team controls
  opp_dummies <- model.matrix(
    ~ opponent - 1,
    data = team_stints
  )

  X <- cbind(
    player_matrix,
    opp_dummies
  )

  y <- (
    team_stints$team_points -
      team_stints$opp_points
  ) / (team_stints$duration_sec / 60)

  w <- team_stints$duration_sec

  list(
    X = X,
    y = y,
    w = w,
    players = all_players,
    stints = team_stints
  )
}

# Fit ridge regression with whole games kept together in cross-validation.
fit_rapm_model <- function(
    stints_df,
    team_name,
    player_lookup,
    lambda_choice = "lambda.1se"
) {

  dm <- build_player_design_matrix(
    stints_df,
    team_name
  )

  if (nrow(dm$X) < 15) {
    warning(
      team_name,
      " has only ",
      nrow(dm$X),
      " stints after filtering."
    )
  }

  game_ids <- unique(dm$stints$fixtureId)

  if (length(game_ids) < 3) {
    stop(
      "At least 3 games are required for game-level cross-validation."
    )
  }

  # Keep every game entirely within one CV fold
  fold_lookup <- setNames(
    seq_along(game_ids),
    game_ids
  )

  fold_id <- unname(
    fold_lookup[dm$stints$fixtureId]
  )

  set.seed(123)

  cvfit <- cv.glmnet(
    x = dm$X,
    y = dm$y,
    alpha = 0,
    weights = dm$w,
    foldid = fold_id
  )

  coefficients <- coef(
    cvfit,
    s = lambda_choice
  )

  # Keep full precision
  player_coefficients <- as.numeric(
    coefficients[dm$players, 1]
  )

  # Prediction based on all five LTU players
  # and the opponent-team control
  predicted <- as.numeric(
    predict(
      cvfit,
      newx = dm$X,
      s = lambda_choice
    )
  )

  stint_predictions <- dm$stints %>%
    mutate(
      actual_net_per_minute = dm$y,

      predicted_net_per_minute =
        predicted,

      stint_residual =
        actual_net_per_minute -
        predicted_net_per_minute
    )

  player_effects <- tibble(
    personId = dm$players,
    rapm_rating = player_coefficients
  ) %>%
    left_join(
      player_lookup,
      by = "personId"
    ) %>%
    select(
      player_name,
      personId,
      rapm_rating
    ) %>%
    arrange(desc(rapm_rating))

  list(
    player_effects = player_effects,
    stint_predictions = stint_predictions,
    cvfit = cvfit,
    lambda_choice = lambda_choice,
    lambda_used = if (
      lambda_choice == "lambda.1se"
    ) {
      cvfit$lambda.1se
    } else {
      cvfit$lambda.min
    }
  )
}

# Return individual coefficients without rounding them.
fit_rapm <- function(stints_df, team_name, player_lookup,
                     lambda_choice = "lambda.1se") {
  fit_rapm_model(
    stints_df, team_name, player_lookup, lambda_choice
  )$player_effects
}

# Average full-lineup residuals during each pair’s shared minutes.
compute_connection_scores <- function(stints_df, team_name, player_lookup,
                                      min_minutes = 15, min_games = 2,
                                      lambda_choice = "lambda.1se") {
  model <- fit_rapm_model(
    stints_df, team_name, player_lookup, lambda_choice
  )

  pair_residuals <- model$stint_predictions %>%
    mutate(
      pairs = map(
        lineup,
        ~ combn(as.character(.x), 2, simplify = FALSE)
      )
    ) %>%
    select(
      fixtureId, duration_sec, actual_net_per_minute,
      predicted_net_per_minute, stint_residual, pairs
    ) %>%
    unnest(pairs) %>%
    mutate(
      id_1 = map_chr(pairs, 1),
      id_2 = map_chr(pairs, 2)
    ) %>%
    group_by(id_1, id_2) %>%
    summarise(
      minutes_together = sum(duration_sec) / 60,
      n_games_together = n_distinct(fixtureId),
      net_per_minute = weighted.mean(
        actual_net_per_minute, duration_sec, na.rm = TRUE
      ),
      expected_net_per_minute = weighted.mean(
        predicted_net_per_minute, duration_sec, na.rm = TRUE
      ),
      connection_score = weighted.mean(
        stint_residual, duration_sec, na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    filter(
      minutes_together >= min_minutes,
      n_games_together >= min_games
    ) %>%
    left_join(player_lookup, by = c("id_1" = "personId")) %>%
    rename(player_1 = player_name) %>%
    left_join(player_lookup, by = c("id_2" = "personId")) %>%
    rename(player_2 = player_name) %>%
    select(
      player_1, player_2, id_1, id_2,
      minutes_together, n_games_together,
      net_per_minute, expected_net_per_minute, connection_score
    ) %>%
    arrange(desc(connection_score))

  attr(pair_residuals, "rapm_model") <- model
  pair_residuals
}

# 6. Functions for pair stability ----

# Remove one game at a time and check whether each score keeps its sign.
leave_one_game_out_stability <- function(stints_df, team_name,
                                         player_lookup,
                                         min_minutes = 15,
                                         min_games = 2,
                                         lambda_choice = "lambda.1se") {
  game_ids <- unique(
    stints_df$fixtureId[stints_df$team == team_name]
  )

  loo_scores <- map_dfr(game_ids, function(game_left_out) {
    training_stints <- stints_df %>%
      filter(fixtureId != game_left_out)

    tryCatch(
      compute_connection_scores(
        training_stints,
        team_name,
        player_lookup,
        min_minutes = min_minutes,
        min_games = min_games,
        lambda_choice = lambda_choice
      ) %>%
        transmute(
          game_left_out,
          id_1, id_2,
          loo_connection_score = connection_score
        ),
      error = function(e) {
        warning("LOO model failed for fixture ", game_left_out,
                ": ", conditionMessage(e))
        tibble()
      }
    )
  })

  loo_scores %>%
    group_by(id_1, id_2) %>%
    summarise(
      n_loo_estimates = n(),
      mean_loo_score = mean(loo_connection_score),
      min_loo_score = min(loo_connection_score),
      max_loo_score = max(loo_connection_score),
      proportion_positive = mean(loo_connection_score > 0),
      .groups = "drop"
    ) %>%
    mutate(
      stability = case_when(
        n_loo_estimates < ceiling(length(game_ids) / 2) ~
          "Insufficient stability evidence",
        proportion_positive >= 0.80 ~ "Stable positive",
        proportion_positive <= 0.20 ~ "Stable negative",
        TRUE ~ "Uncertain"
      )
    )
}

# Join scores and stability; round only the presentation table.
make_final_pair_table <- function(connection_scores, stability_results) {
  connection_scores %>%
    left_join(stability_results, by = c("id_1", "id_2")) %>%
    mutate(
      stability = replace_na(stability, "Insufficient stability evidence")
    ) %>%
    select(
      player_1, player_2, minutes_together, n_games_together,
      net_per_minute, expected_net_per_minute, connection_score,
      stability, n_loo_estimates, proportion_positive,
      min_loo_score, max_loo_score
    ) %>%
    mutate(
      across(
        c(minutes_together, net_per_minute,
          expected_net_per_minute, connection_score,
          proportion_positive, min_loo_score, max_loo_score),
        ~ round(.x, 2)
      )
    ) %>%
    arrange(desc(connection_score))
}

# 7. Functions for minutes and connectivity ----

# Add up reconstructed playing time for each LTU player.
compute_player_minutes <- function(stints_df, team_name, player_lookup) {
  team_stints <- stints_df %>% filter(team == team_name)
  team_stints %>%
    mutate(stint_id = row_number()) %>%
    select(stint_id, lineup, duration_sec) %>%
    unnest(lineup) %>%
    rename(personId = lineup) %>%
    group_by(personId) %>%
    summarise(total_minutes = round(sum(duration_sec) / 60, 1), .groups = "drop") %>%
    left_join(player_lookup, by = "personId") %>%
    select(player_name, personId, total_minutes) %>%
    arrange(desc(total_minutes))
}

# Summarise eligible pair scores for each player, weighted by shared minutes.
summarise_player_connectivity <- function(ltu_connection) {
  bind_rows(
    ltu_connection %>% select(player = player_1, connection_score, minutes_together),
    ltu_connection %>% select(player = player_2, connection_score, minutes_together)
  ) %>%
    group_by(player) %>%
    summarise(
      n_qualifying_pairs = n(),
      weighted_connection_score = weighted.mean(
        connection_score,
        w = minutes_together,
        na.rm = TRUE
      ),
      cumulative_pair_minutes = sum(minutes_together),
      .groups = "drop"
    ) %>%
    mutate(
      weighted_connection_score = round(weighted_connection_score, 2),
      cumulative_pair_minutes = round(cumulative_pair_minutes, 1)
    ) %>%
    arrange(desc(weighted_connection_score))
}

# 8. Functions for season network plots ----

# Edge colour is the connection score; width is shared minutes.
# Node positions are for display and do not represent court positions.
build_connection_network_plot <- function(ltu_connection, ltu_minutes,
                                          player_labels = NULL) {

  edges <- ltu_connection %>%
    rename(
      from = player_1,
      to = player_2,
      conn_score = connection_score
    )

  nodes <- ltu_minutes %>%
    rename(name = player_name) %>%
    filter(name %in% union(edges$from, edges$to))

  nodes$display_label <- nodes$name
  if (!is.null(player_labels)) {
    nodes$display_label <- unname(player_labels[str_squish(nodes$name)])
    if (anyNA(nodes$display_label)) stop("Some players have no presentation label.")
  }
  label_size <- if (is.null(player_labels)) 3.4 else 4.2

  g <- graph_from_data_frame(
    edges,
    directed = FALSE,
    vertices = nodes
  )

  set.seed(123)

  ggraph(g, layout = "fr") +
    geom_edge_link(
      aes(
        width = minutes_together,
        colour = conn_score,
        alpha = n_games_together
      )
    ) +
    geom_node_point(
      aes(size = total_minutes),
      colour = "grey20"
    ) +
    geom_node_label(
      aes(label = display_label),
      repel = TRUE,
      size = label_size,
      colour = "black",
      fill = alpha("white", 0.9),
      label.padding = unit(0.15, "lines"),
      label.size = 0
    ) +
    scale_edge_width(
      range = c(0.4, 4),
      name = "Minutes together"
    ) +
    scale_edge_colour_gradient2(
      low = "firebrick",
      mid = "grey80",
      high = "steelblue",
      midpoint = 0,
      name = "Connection score"
    ) +
    scale_edge_alpha(
      range = c(0.25, 0.9),
      name = "Games together"
    ) +
    scale_size_continuous(
      range = c(5, 15),
      name = "Player minutes"
    ) +
    theme_void() +
    labs(
      title = "LTU adjusted player-pair connection network",
      subtitle = paste(
        "Edge width = minutes shared |",
        "Blue = positive connection |",
        "Red = negative connection"
      )
    ) +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 11),
      plot.margin = margin(20, 30, 20, 30)
    )
}

build_connection_network_plot_presentation <- function(
    ltu_connection, ltu_minutes, player_labels) {
  build_connection_network_plot(ltu_connection, ltu_minutes, player_labels)
}

build_interactive_connection_network <- function(ltu_connection,
                                                 ltu_minutes) {

  # Edge information
  edges <- ltu_connection %>%
    transmute(
      from = player_1,
      to = player_2,

      # Thicker line = more minutes together
      width = rescale(minutes_together, to = c(1, 8)),

      # Blue positive; red negative
      color = if_else(
        connection_score >= 0,
        alpha("steelblue", 0.75),
        alpha("firebrick", 0.75)
      ),

      title = paste0(
        "<b>", player_1, " and ", player_2, "</b>",
        "<br>Connection score: ",
        round(connection_score, 2),
        "<br>Minutes together: ",
        round(minutes_together, 1),
        "<br>Games together: ",
        n_games_together
      ),

      value = minutes_together
    )

  # Player information
  nodes <- ltu_minutes %>%
    filter(player_name %in% union(edges$from, edges$to)) %>%
    transmute(
      id = player_name,
      label = player_name,

      # Larger node = more playing time
      value = total_minutes,

      title = paste0(
        "<b>", player_name, "</b>",
        "<br>Total minutes: ",
        round(total_minutes, 1)
      ),

      color = "#343434",
      font.color = "#111111",
      font.size = 22,
      font.background = "rgba(255,255,255,0.85)"
    )

  visNetwork::visNetwork(
    nodes = nodes,
    edges = edges,
    width = "100%",
    height = "800px",
    main = "LTU adjusted player-pair connection network"
  ) %>%
    visNetwork::visNodes(
      shape = "dot",
      scaling = list(min = 12, max = 35)
    ) %>%
    visNetwork::visEdges(
      smooth = FALSE,
      scaling = list(min = 1, max = 8)
    ) %>%
    visNetwork::visPhysics(
      solver = "forceAtlas2Based",
      forceAtlas2Based = list(
        gravitationalConstant = -80,
        centralGravity = 0.01,
        springLength = 180,
        springConstant = 0.04,
        avoidOverlap = 1
      ),
      stabilization = list(iterations = 1000)
    ) %>%
    visNetwork::visEvents(
      stabilizationIterationsDone = "
    function () {
      this.setOptions({physics: false});
      this.fit();
    }
  "
    ) %>%
    visNetwork::visOptions(
      highlightNearest = list(
        enabled = TRUE,
        degree = 1,
        hover = TRUE
      ),
      nodesIdSelection = TRUE
    ) %>%
    visNetwork::visInteraction(
      hover = TRUE,
      navigationButtons = TRUE,
      keyboard = TRUE
    )
}

# 9. Functions for the usage network ----

# Build the separate network of shared playing time.
compute_pair_minutes_network <- function(stints_df, team_name, player_lookup,
                                         min_minutes = 1, min_games = 1) {
  pair_minutes <- stints_df %>%
    filter(team == team_name, duration_sec > 0, map_int(lineup, length) == 5) %>%
    mutate(pairs = map(lineup, ~ combn(sort(as.character(.x)), 2, simplify = FALSE))) %>%
    select(fixtureId, duration_sec, pairs) %>%
    unnest(pairs) %>%
    mutate(id_1 = map_chr(pairs, 1), id_2 = map_chr(pairs, 2)) %>%
    select(-pairs) %>%
    group_by(id_1, id_2) %>%
    summarise(
      minutes_together = sum(duration_sec) / 60,
      n_games_together = n_distinct(fixtureId),
      .groups = "drop"
    ) %>%
    filter(minutes_together >= min_minutes, n_games_together >= min_games) %>%
    left_join(player_lookup, by = c("id_1" = "personId")) %>% rename(player_1 = player_name) %>%
    left_join(player_lookup, by = c("id_2" = "personId")) %>% rename(player_2 = player_name) %>%
    filter(!is.na(player_1), !is.na(player_2)) %>%
    arrange(desc(minutes_together))
  pair_minutes
}

# Measure how concentrated a non-negative set of values is.
gini_coefficient <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) == 0 || sum(x) == 0) return(0)
  x <- sort(x)
  n <- length(x)
  sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))
}

# Shared minutes are strengths; their inverses are shortest-path distances.
# Betweenness describes the rotation network, not passing or leadership.
calculate_centralisation <- function(pair_minutes, network_label = "2025") {
  if (nrow(pair_minutes) == 0) {
    stop("The pair-minutes table contains no qualifying connections.")
  }

  edge_table <- pair_minutes %>%
    transmute(from = as.character(id_1), to = as.character(id_2), minutes_together)

  vertex_table <- bind_rows(
    pair_minutes %>% transmute(name = as.character(id_1), player_name = player_1),
    pair_minutes %>% transmute(name = as.character(id_2), player_name = player_2)
  ) %>% distinct(name, .keep_all = TRUE)

  network_graph <- graph_from_data_frame(d = edge_table, directed = FALSE, vertices = vertex_table)

  E(network_graph)$weight <- E(network_graph)$minutes_together
  E(network_graph)$distance <- 1 / E(network_graph)$minutes_together

  player_degree <- degree(network_graph, mode = "all")
  player_strength <- strength(network_graph, weights = E(network_graph)$weight)
  weighted_betweenness_raw <- betweenness(network_graph, directed = FALSE,
                                          weights = E(network_graph)$distance, normalized = FALSE)
  weighted_betweenness_normalised <- betweenness(network_graph, directed = FALSE,
                                                 weights = E(network_graph)$distance, normalized = TRUE)
  player_closeness <- closeness(network_graph, mode = "all",
                                weights = E(network_graph)$distance, normalized = TRUE)
  player_eigenvector <- eigen_centrality(network_graph, directed = FALSE,
                                         weights = E(network_graph)$weight)$vector

  V(network_graph)$weighted_strength <- as.numeric(player_strength)
  V(network_graph)$weighted_betweenness <- as.numeric(weighted_betweenness_normalised)

  player_centrality <- tibble(
    network = as.character(network_label),
    personId = V(network_graph)$name,
    player_name = V(network_graph)$player_name,
    degree = as.numeric(player_degree),
    weighted_strength = as.numeric(player_strength),
    weighted_betweenness = as.numeric(weighted_betweenness_normalised),
    weighted_closeness = as.numeric(player_closeness),
    eigenvector_centrality = as.numeric(player_eigenvector)
  ) %>% arrange(desc(weighted_strength))

  n_players <- vcount(network_graph)
  strength_mean <- mean(player_strength)
  strength_sd <- sd(player_strength)
  strength_cv <- if (strength_mean > 0) strength_sd / strength_mean else NA_real_
  strength_gini <- gini_coefficient(player_strength)
  weighted_betweenness_gini <- gini_coefficient(weighted_betweenness_raw)

  unweighted_betweenness <- betweenness(network_graph, directed = FALSE, weights = NA, normalized = FALSE)
  freeman_numerator <- sum(max(unweighted_betweenness) - unweighted_betweenness)
  freeman_denominator <- if (n_players > 2) ((n_players - 1)^2 * (n_players - 2)) / 2 else NA_real_
  freeman_betweenness_centralisation <- freeman_numerator / freeman_denominator

  network_centralisation <- tibble(
    network = as.character(network_label),
    n_players = n_players,
    n_edges = ecount(network_graph),
    network_density = edge_density(network_graph, loops = FALSE),
    mean_weighted_strength = mean(player_strength),
    strength_cv = strength_cv,
    strength_gini = strength_gini,
    weighted_betweenness_gini = weighted_betweenness_gini,
    freeman_betweenness_centralisation = freeman_betweenness_centralisation
  )

  list(graph = network_graph, player_centrality = player_centrality,
       network_centralisation = network_centralisation)
}

# Check how often each player appears across games.
compute_player_game_participation <- function(stints_df, team_name, player_lookup) {
  team_stints <- stints_df %>% filter(team == team_name)
  team_stints %>%
    mutate(stint_id = row_number()) %>%
    select(stint_id, fixtureId, lineup, duration_sec) %>%
    unnest(lineup) %>%
    rename(personId = lineup) %>%
    group_by(personId) %>%
    summarise(
      n_games_played = n_distinct(fixtureId),
      total_minutes = round(sum(duration_sec) / 60, 1),
      avg_minutes_per_game = round(total_minutes / n_games_played, 1),
      .groups = "drop"
    ) %>%
    left_join(player_lookup, by = "personId") %>%
    select(player_name, n_games_played, total_minutes, avg_minutes_per_game) %>%
    arrange(n_games_played, total_minutes)
}

# 10. Functions for game situations ----

# Use the final active definition from the original script.
# Classifications use the score and clock at stint start; stints are not split.
add_game_state_variables <- function(stints_df, expected_periods = 4, period_length = PERIOD_LENGTH_SECONDS) {
  stints_df %>%
    mutate(
      game_seconds_remaining = (expected_periods - period) * period_length + start_clock_seconds,
      score_state = case_when(
        score_margin_start >= 6  ~ "Leading by 6+",
        score_margin_start <= -6 ~ "Trailing by 6+",
        TRUE                     ~ "Within 5 points"
      ),
      game_phase = case_when(
        period <= 2 ~ "First half",
        period == 3 ~ "Third quarter",
        period == 4 & start_clock_seconds > 300 ~ "Early fourth quarter",
        period == 4 & start_clock_seconds <= 300 ~ "Final five minutes",
        period > 4 ~ "Overtime"
      ),
      late_game = period == 4 & start_clock_seconds <= 300,
      clutch_situation = late_game & abs(score_margin_start) <= 5
    )
}

# Turn a lineup of player IDs into one readable label.
lineup_to_names <- function(personId_vec, player_lookup) {
  nm <- player_lookup$player_name[match(personId_vec, player_lookup$personId)]
  paste(sort(nm), collapse = " | ")
}

# 11. Download the season data ----

url <- "https://embed-api.eui.connect.sportradar.com/v1/embed/303/fixtures?state=eJyrVipWslJQMjMwTjQ3T7bUNbM0sNA1NEwz0LVMSjbVtUwxTDM1N0hKSjQxVNJRUMoBKU7N03X1U6oFAK4sDo8"

res <- GET(url, add_headers("User-Agent" = "Mozilla/5.0"))
stop_for_status(res)

fixtures_data <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)

# Build a clean fixtures table: extract state string + team names
fixtures_df <- fixtures_data$data$fixtures %>%
  mutate(
    state = str_extract(link, "(?<=~w=f~).+"),
    team1 = map_chr(competitors, ~ .x$name[1]),
    team2 = map_chr(competitors, ~ .x$name[2])
  )

# Download play-by-play for all LTU fixtures.

match_pick <- fixtures_df %>%
  filter(team1 == ltu_settings$team | team2 == ltu_settings$team)

LTU_pbp <- lapply(match_pick$state, function(s) {
  Sys.sleep(0.5)
  get_match_pbp(s)
}) %>% bind_rows()
LTU_pbp <- LTU_pbp %>%
  left_join(match_pick %>% select(fixtureId, round, startTimeLocal), by = "fixtureId")

if (nrow(LTU_pbp) == 0) stop("No LTU play-by-play was downloaded.")
# These downloads can change if the provider updates the feed.
# Keep the extraction date and data snapshot used for the final report.

# 12. Reconstruct lineups and estimate connection scores ----

player_lookup <- build_player_lookup(
  LTU_pbp
)

stints_df <- build_stints(
  LTU_pbp
)

stints_df_clean <- exclude_broken_games(
  stints_df,
  team_name = ltu_settings$team,
  min_pct = ltu_settings$min_coverage
)

nrow(stints_df_clean)

reconstruction_checks <- validate_reconstruction(
  stints_df = stints_df_clean,
  pbp_df = LTU_pbp,
  team_name = ltu_settings$team
)

ltu_ratings <- fit_rapm(
  stints_df = stints_df_clean,
  team_name = ltu_settings$team,
  player_lookup = player_lookup,
  lambda_choice = ltu_settings$lambda_choice
)

ltu_connection <- compute_connection_scores(
  stints_df = stints_df_clean,
  team_name = ltu_settings$team,
  player_lookup = player_lookup,
  min_minutes = ltu_settings$min_pair_minutes,
  min_games = ltu_settings$min_pair_games,
  lambda_choice = ltu_settings$lambda_choice
)

ltu_stability <- leave_one_game_out_stability(
  stints_df = stints_df_clean,
  team_name = ltu_settings$team,
  player_lookup = player_lookup,
  min_minutes = ltu_settings$min_pair_minutes,
  min_games = ltu_settings$min_pair_games,
  lambda_choice = ltu_settings$lambda_choice
)

final_pair_table <- make_final_pair_table(
  ltu_connection,
  ltu_stability
)

print(final_pair_table)

print(reconstruction_checks)
print(final_pair_table %>% count(stability))
# This model is reused by the game-level visualisation and report snapshot.
season_model <- attr(ltu_connection, "rapm_model")

# 13. Summarise player minutes and selected pairs ----

ltu_minutes <- compute_player_minutes(stints_df_clean, ltu_settings$team, player_lookup)
player_connectivity <- summarise_player_connectivity(ltu_connection)

stable_positive_pairs <- final_pair_table %>%
  filter(stability == "Stable positive") %>%
  slice_max(connection_score, n = 5, with_ties = FALSE)
stable_negative_pairs <- final_pair_table %>%
  filter(stability == "Stable negative") %>%
  slice_min(connection_score, n = 5, with_ties = FALSE)

print(ltu_minutes)
print(player_connectivity)
print(stable_positive_pairs)
print(stable_negative_pairs)

# 14. Draw and save the season networks ----

player_names <- sort(unique(
  stringr::str_squish(ltu_minutes$player_name)
))

player_names <- player_names[
  !is.na(player_names) & nzchar(player_names)
]

player_labels <- setNames(
  paste("Player", seq_along(player_names)),
  player_names
)

static_network <- build_connection_network_plot(ltu_connection, ltu_minutes)
network_plot <- static_network
static_network_presentation <- build_connection_network_plot_presentation(
  ltu_connection, ltu_minutes, player_labels
)
print(static_network)
print(static_network_presentation)

ggsave(
  filename = file.path(ltu_paths$figures, "LTU_connection_network.png"),
  plot = static_network, width = 10, height = 8, dpi = 300, bg = "white"
)
ggsave(
  filename = file.path(ltu_paths$figures, "LTU_connection_network_presentation.png"),
  plot = static_network_presentation, width = 12, height = 7, dpi = 300, bg = "white"
)

# The static analysis can still run without the optional interactive package.
interactive_network <- NULL
if (requireNamespace("visNetwork", quietly = TRUE)) {
  interactive_network <- build_interactive_connection_network(ltu_connection, ltu_minutes)
  if (interactive()) print(interactive_network)
} else {
  message("Optional interactive network skipped. Install visNetwork to view it.")
}

# 15. Describe the usage network ----

pair_minutes_2025 <- compute_pair_minutes_network(
  stints_df = stints_df_clean, team_name = ltu_settings$team, player_lookup = player_lookup,
  min_minutes = 1, min_games = 1
)

centrality_2025 <- calculate_centralisation(pair_minutes_2025, network_label = "LTU 2025")

centrality_2025$network_centralisation %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  as.data.frame() %>% print()

centrality_2025$player_centrality %>%
  select(player_name, degree, weighted_strength, weighted_betweenness,
         weighted_closeness, eigenvector_centrality) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(desc(weighted_betweenness)) %>% print()

top_bridging_players <- centrality_2025$player_centrality %>%
  slice_max(weighted_betweenness, n = 10, with_ties = FALSE)

bridging_plot <- ggplot(top_bridging_players, aes(x = reorder(player_name, weighted_betweenness), y = weighted_betweenness)) +
  geom_col(fill = "lightblue3") +
  coord_flip() +
  theme_classic(base_size = 14) +
  labs(
    title = "LTU players by weighted betweenness",
    subtitle = "Stronger shared-minute connections are treated as shorter network distances",
    x = "Player", y = "Weighted betweenness"
  )

print(bridging_plot)
ggsave(
  filename = file.path(ltu_paths$figures, "LTU_weighted_betweenness.png"),
  plot = bridging_plot, width = 10, height = 7, dpi = 300, bg = "white"
)

# 16. Check missing pairs and game participation ----

all_players <- unique(c(pair_minutes_2025$player_1, pair_minutes_2025$player_2))
length(all_players)

all_possible <- combn(sort(all_players), 2, simplify = FALSE)

existing_pairs <- pair_minutes_2025 %>%
  transmute(a = pmin(player_1, player_2), b = pmax(player_1, player_2))

missing_pairs <- tibble(
  a = map_chr(all_possible, 1),
  b = map_chr(all_possible, 2)
) %>%
  anti_join(existing_pairs, by = c("a", "b"))

nrow(missing_pairs)
print(missing_pairs)

bind_rows(
  missing_pairs %>% select(player = a),
  missing_pairs %>% select(player = b)
) %>% count(player, sort = TRUE)

game_participation <- compute_player_game_participation(
  stints_df_clean, ltu_settings$team, player_lookup
)
print(game_participation)

# 17. Summarise situational deployment ----

enriched_stints <- add_game_state_variables(stints_df_clean)

# Check the available score-state and clutch coverage.
score_margin_check <- enriched_stints %>%
  filter(team == ltu_settings$team) %>%
  summarise(
    min_margin = min(score_margin_start),
    max_margin = max(score_margin_start),
    missing_margin = sum(is.na(score_margin_start))
  )

clutch_coverage <- enriched_stints %>%
  filter(team == ltu_settings$team, clutch_situation) %>%
  summarise(n_stints = n(), total_minutes = sum(duration_sec) / 60, n_games = n_distinct(fixtureId))

score_state_coverage <- enriched_stints %>%
  filter(team == ltu_settings$team) %>%
  group_by(score_state) %>%
  summarise(n_stints = n(), total_minutes = sum(duration_sec) / 60, n_games = n_distinct(fixtureId), .groups = "drop")

print(score_margin_check)
print(clutch_coverage)
print(score_state_coverage)

# Summarise each lineup within a score state.
situational_lineup_summary <- enriched_stints %>%
  filter(team == ltu_settings$team) %>%
  mutate(lineup_name = map_chr(lineup, ~ lineup_to_names(.x, player_lookup))) %>%
  group_by(lineup_name, score_state) %>%
  summarise(
    minutes = sum(duration_sec) / 60,
    games = n_distinct(fixtureId),
    n_stints = n(),
    points_for = sum(team_points),
    points_against = sum(opp_points),
    net_points = points_for - points_against,
    .groups = "drop"
  ) %>%
  mutate(
    net_per_minute = if_else(minutes > 0, net_points / minutes, NA_real_),
    evidence_level = if_else(minutes >= 5 & games >= 2, "Adequate", "Limited")
  ) %>%
  arrange(desc(minutes))

lineups_with_adequate_exposure <- situational_lineup_summary %>%
  filter(evidence_level == "Adequate")

print(situational_lineup_summary)
print(lineups_with_adequate_exposure)

# Summarise each lineup within a game phase.
lineup_by_game_phase <- enriched_stints %>%
  filter(team == ltu_settings$team) %>%
  mutate(lineup_name = map_chr(lineup, ~ lineup_to_names(.x, player_lookup))) %>%
  group_by(lineup_name, game_phase) %>%
  summarise(
    minutes = sum(duration_sec) / 60,
    games = n_distinct(fixtureId),
    n_stints = n(),
    net_points = sum(team_points) - sum(opp_points),
    .groups = "drop"
  ) %>%
  mutate(net_per_minute = if_else(minutes > 0, net_points / minutes, NA_real_)) %>%
  arrange(desc(minutes))

print(lineup_by_game_phase)

# Summarise late-game and clutch lineups.
# These are descriptive summaries. Check games and minutes before interpreting them.
late_game_lineups <- enriched_stints %>%
  filter(team == ltu_settings$team, late_game) %>%
  mutate(lineup_name = map_chr(lineup, ~ lineup_to_names(.x, player_lookup))) %>%
  group_by(lineup_name) %>%
  summarise(
    minutes = sum(duration_sec) / 60,
    games = n_distinct(fixtureId),
    n_stints = n(),
    net_points = sum(team_points) - sum(opp_points),
    .groups = "drop"
  ) %>%
  mutate(net_per_minute = if_else(minutes > 0, net_points / minutes, NA_real_)) %>%
  arrange(desc(minutes))

clutch_lineups <- enriched_stints %>%
  filter(team == ltu_settings$team, clutch_situation) %>%
  mutate(lineup_name = map_chr(lineup, ~ lineup_to_names(.x, player_lookup))) %>%
  group_by(lineup_name) %>%
  summarise(
    minutes = sum(duration_sec) / 60,
    games = n_distinct(fixtureId),
    n_stints = n(),
    net_points = sum(team_points) - sum(opp_points),
    .groups = "drop"
  ) %>%
  mutate(net_per_minute = if_else(minutes > 0, net_points / minutes, NA_real_)) %>%
  arrange(desc(minutes))

print(late_game_lineups)
print(clutch_lineups)

# Check which situations each pair played in.
pair_by_score_state <- enriched_stints %>%
  filter(team == ltu_settings$team) %>%
  mutate(pairs = map(lineup, ~ combn(sort(as.character(.x)), 2, simplify = FALSE))) %>%
  select(fixtureId, duration_sec, score_state, pairs) %>%
  unnest(pairs) %>%
  mutate(id_1 = map_chr(pairs, 1), id_2 = map_chr(pairs, 2)) %>%
  group_by(id_1, id_2, score_state) %>%
  summarise(minutes = sum(duration_sec) / 60, games = n_distinct(fixtureId), .groups = "drop") %>%
  left_join(player_lookup, by = c("id_1" = "personId")) %>% rename(player_1 = player_name) %>%
  left_join(player_lookup, by = c("id_2" = "personId")) %>% rename(player_2 = player_name) %>%
  select(player_1, player_2, score_state, minutes, games) %>%
  pivot_wider(
    names_from = score_state,
    values_from = c(minutes, games),
    values_fill = 0
  )

# Join situational usage to the season connection scores.
connection_usage_by_score <- ltu_connection %>%
  left_join(pair_by_score_state, by = c("player_1", "player_2")) %>%
  arrange(desc(connection_score))

print(connection_usage_by_score)

# Collect timeout events from the feed.
timeout_events <- LTU_pbp %>%
  filter(action == "timeOut") %>%
  select(fixtureId, period, clock_seconds, team, action, detail)

print(timeout_events)

# 18. Save the team tables ----

write_ltu_table(reconstruction_checks, "LTU_reconstruction_checks.csv")
write_ltu_table(ltu_ratings, "LTU_rapm_ratings_full_precision.csv")
write_ltu_table(final_pair_table, "LTU_final_pair_connection_table.csv")
write_ltu_table(player_connectivity, "LTU_player_connectivity_summary.csv")
write_ltu_table(situational_lineup_summary, "LTU_situational_lineup_summary.csv")
write_ltu_table(lineup_by_game_phase, "LTU_lineup_by_game_phase.csv")
write_ltu_table(late_game_lineups, "LTU_late_game_lineups.csv")
write_ltu_table(clutch_lineups, "LTU_clutch_lineups.csv")
write_ltu_table(connection_usage_by_score, "LTU_connection_usage_by_score.csv")
write_ltu_table(timeout_events, "LTU_timeout_events.csv")
write_ltu_table(centrality_2025$player_centrality, "LTU_centrality_player.csv")
write_ltu_table(centrality_2025$network_centralisation, "LTU_centrality_network.csv")
write_ltu_table(score_state_coverage, "LTU_score_state_coverage.csv")
write_ltu_table(clutch_coverage, "LTU_clutch_coverage.csv")
write_ltu_table(game_participation, "LTU_player_game_participation.csv")
message("Team analysis finished. Tables: outputs/tables/; plots: Figures/.")
