library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(glmnet)

PERIOD_LENGTH_SECONDS <- 600
SCORE_POINTS <- c("2pt" = 2, "3pt" = 3, "freeThrow" = 1)


## ------------------------------------------------------------
## 1. DATA ACQUISITION
## ------------------------------------------------------------

# parse ISO 8601 clock (e.g. "PT9M45S") into seconds remaining
parse_clock <- function(x) {
  mins <- as.numeric(sub("PT(\\d+)M.*", "\\1", x))
  secs <- as.numeric(sub(".*M(\\d+(?:\\.\\d+)?)S", "\\1", x))
  mins * 60 + secs
}

# pull clean play-by-play for one fixture
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
  
  if (is.null(data$data$pbp)) {
    warning("Still no pbp available for: ", fixture_state)
    return(NULL)
  }
  
  all_events <- data$data$pbp |>
    lapply(\(quarter) quarter$events) |>
    bind_rows(.id = "quarter")
  
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

## --- Fetch full season fixtures list ---
url <- "https://embed-api.eui.connect.sportradar.com/v1/embed/303/fixtures?state=eJyrVipWslJQMjMwTjQ3T7bUNbM0sNA1NEwz0LVMSjbVtUwxTDM1N0hKSjQxVNJRUMoBKU7N03X1U6oFAK4sDo8"

res <- GET(url, add_headers("User-Agent" = "Mozilla/5.0"))
status_code(res)   # should be 200

fixtures_data <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)

# Build a clean fixtures table: extract state string + team names 
fixtures_df <- fixtures_data$data$fixtures %>%
  mutate(
    state = str_extract(link, "(?<=~w=f~).+"),
    team1 = map_chr(competitors, ~ .x$name[1]),
    team2 = map_chr(competitors, ~ .x$name[2])
  )

## --- Pull LTU's full season play-by-play ---
match_pick <- fixtures_df %>%
  filter((team1 == "LTU") | (team2 == "LTU"))

LTU_pbp <- lapply(match_pick$state, function(s) {
  Sys.sleep(0.5)
  get_match_pbp(s)
}) %>% bind_rows()
LTU_pbp <- LTU_pbp %>%
  left_join(match_pick %>% select(fixtureId, round, startTimeLocal), by = "fixtureId")


## ------------------------------------------------------------
## 0. Player ID <-> name lookup
## ------------------------------------------------------------
#' One row per personId with a display name (first non-NA name seen).
#' Build this once from the raw pbp and carry it through every output
#' table so results stay human-readable while the model itself keys
#' on personId, not the (potentially ambiguous) name string.
build_player_lookup <- function(pbp_df) {
  pbp_df %>%
    filter(!is.na(personId), !is.na(player)) %>%
    mutate(personId = as.character(personId)) %>%
    group_by(personId) %>%
    summarise(player_name = first(player), .groups = "drop")
}


## ------------------------------------------------------------
## 1. Starting lineup detection + quarter-break validation
## ------------------------------------------------------------
#' Starters inferred as the first 5 distinct personIds from `team_name`
#' with events logged before that team's first substitution.
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

#' For periods after the first, the lineup is carried forward from the
#' end of the previous period (see build_stints). This function checks
#' that assumption: it looks at who had logged actions before the
#' first substitution of the new period and flags any player NOT in
#' the carried-over lineup — that's a sign of an un-logged quarter-break
#' change (e.g. a coach's substitution the data didn't record as a
#' "substitution" event). This does NOT correct the lineup automatically
#' (we have no reliable alternative source of truth) — it surfaces the
#' discrepancy so it can be documented as a limitation.
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


## ------------------------------------------------------------
## 2. Build stints
##    Adds exact stint clocks and cumulative score at stint start
## ------------------------------------------------------------
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


## ------------------------------------------------------------
## 2A. Add score-state, game-phase and clutch variables
## ------------------------------------------------------------
add_game_state_variables <- function(
    stints_df,
    expected_periods = 4,
    period_length = PERIOD_LENGTH_SECONDS
) {
  stints_df %>%
    mutate(
      # Regulation time remaining. Overtime uses the period clock only.
      game_seconds_remaining = if_else(
        period <= expected_periods,
        (expected_periods - period) * period_length +
          start_clock_seconds,
        start_clock_seconds
      ),
      
      score_state = case_when(
        score_margin_start >= 6 ~ "Leading by 6+",
        score_margin_start <= -6 ~ "Trailing by 6+",
        TRUE ~ "Within 5 points"
      ),
      
      score_state = factor(
        score_state,
        levels = c(
          "Leading by 6+",
          "Within 5 points",
          "Trailing by 6+"
        )
      ),
      
      game_phase = case_when(
        period <= 2 ~ "First half",
        period == 3 ~ "Third quarter",
        period == 4 & start_clock_seconds > 300 ~
          "Early fourth quarter",
        period == 4 & start_clock_seconds <= 300 ~
          "Final five minutes",
        period > 4 ~ "Overtime",
        TRUE ~ NA_character_
      ),
      
      late_game =
        period == 4 &
        start_clock_seconds <= 300,
      
      clutch_situation =
        late_game &
        abs(score_margin_start) <= 5
    )
}


## ------------------------------------------------------------
## 3. Naive pair performance (keyed on personId, joined to names at the end)
## ------------------------------------------------------------
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


## ------------------------------------------------------------
## 4. RAPM-style ridge regression, fold-by-game CV, personId-keyed
## ------------------------------------------------------------
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

fit_rapm <- function(stints_df, team_name, player_lookup,
                     lambda_choice = "lambda.1se") {
  fit_rapm_model(
    stints_df, team_name, player_lookup, lambda_choice
  )$player_effects
}


## ------------------------------------------------------------
## 5. PRIMARY OUTPUT: complete-lineup residual connection score
## ------------------------------------------------------------
#' Duration-weighted mean RAPM residual for stints in which both
#' players appeared. Positive means LTU performed above the full-lineup
#' prediction; negative means below prediction. This is an association,
#' not causal proof of chemistry.
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


compute_game_coverage <- function(stints_df, team_name, expected_periods = 4, period_length = 600) {
  stints_df %>%
    filter(team == team_name) %>%
    group_by(fixtureId) %>%
    summarise(total_sec = sum(duration_sec), .groups = "drop") %>%
    mutate(pct_of_full_game = round(total_sec / (expected_periods * period_length), 2)) %>%
    arrange(pct_of_full_game)
}

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


## ------------------------------------------------------------
## 6. RECONSTRUCTION QUALITY CHECKS
## ------------------------------------------------------------
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


## ------------------------------------------------------------
## 7. LEAVE-ONE-GAME-OUT STABILITY
## ------------------------------------------------------------
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




## ------------------------------------------------------------
## USAGE
## ------------------------------------------------------------
player_lookup <- build_player_lookup(
  LTU_pbp
)

stints_df <- build_stints(
  LTU_pbp
)

stints_df_clean <- exclude_broken_games(
  stints_df,
  team_name = "LTU",
  min_pct = 0.50
)

nrow(stints_df_clean)

reconstruction_checks <- validate_reconstruction(
  stints_df = stints_df_clean,
  pbp_df = LTU_pbp,
  team_name = "LTU"
)

ltu_ratings <- fit_rapm(
  stints_df = stints_df_clean,
  team_name = "LTU",
  player_lookup = player_lookup,
  lambda_choice = "lambda.1se"
)

ltu_connection <- compute_connection_scores(
  stints_df = stints_df_clean,
  team_name = "LTU",
  player_lookup = player_lookup,
  min_minutes = 15,
  min_games = 2,
  lambda_choice = "lambda.1se"
)

ltu_stability <- leave_one_game_out_stability(
  stints_df = stints_df_clean,
  team_name = "LTU",
  player_lookup = player_lookup,
  min_minutes = 15,
  min_games = 2,
  lambda_choice = "lambda.1se"
)

final_pair_table <- make_final_pair_table(
  ltu_connection,
  ltu_stability
)


print(final_pair_table)

## print(ltu_connection)
## write.csv(ltu_connection, "LTU_connection_scores.csv", row.names = FALSE)
##
## --- network visualisation (edges = pairs, weight = connection_score) ---
## library(igraph); library(ggraph)
## edges <- ltu_connection %>% rename(from = player_1, to = player_2, weight = connection_score)
## g <- graph_from_data_frame(edges, directed = FALSE)
## E(g)$width <- scales::rescale(edges$minutes_together, to = c(0.5, 4))
## E(g)$color <- ifelse(edges$weight >= 0, "steelblue", "firebrick")
## ggraph(g, layout = "fr") +
##   geom_edge_link(aes(width = width, color = color), alpha = 0.7) +
##   geom_node_point(size = 6, color = "grey20") +
##   geom_node_text(aes(label = name), repel = TRUE, size = 3) +
##   scale_edge_width_identity() + scale_edge_color_identity() +
##   theme_void() +
##   labs(title = "LTU adjusted player-pair connection network")



# Starting lineup detection failed for 2 of 10 games, where a true starter had no recorded action before 
# the game's first substitution; these games were excluded from the player-connection analysis, leaving 8 games for the final results.


## Two of ten games (20%) were excluded from the player-connection analysis after diagnostic tracing revealed that starting-lineup detection 
## failed when a genuine starter had no recorded action before their team's first substitution — this caused lineup-tracking to drift and corrupt 
## the remainder of the game. The eight retained games each preserved 79–100% of expected game-time in the reconstructed stint data.

## Re-run the final tables before retaining any earlier named finding:
## the complete-lineup residual score and two-game threshold may change
## which pairs appear strongest.


# network analysis 
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

build_connection_network_plot <- function(ltu_connection, ltu_minutes) {
  library(igraph); library(ggraph); library(scales)
  
  edges <- ltu_connection %>%
    rename(from = player_1, to = player_2, conn_score = connection_score)
  
  nodes <- ltu_minutes %>%
    rename(name = player_name) %>%
    filter(name %in% union(edges$from, edges$to))
  
  g <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
  
  E(g)$width <- rescale(edges$minutes_together, to = c(0.4, 4))
  E(g)$color <- ifelse(edges$conn_score >= 0, "steelblue", "firebrick")
  E(g)$alpha <- rescale(edges$n_games_together, to = c(0.3, 0.9))
  V(g)$size  <- rescale(nodes$total_minutes, to = c(6, 16))
  
  ggraph(g, layout = "fr") +
    geom_edge_link(aes(width = width, color = color, alpha = alpha)) +
    geom_node_point(aes(size = size), color = "grey20") +
    geom_node_text(aes(label = name), repel = TRUE, size = 3.2) +
    scale_edge_width_identity() +
    scale_edge_color_identity() +
    scale_edge_alpha_identity() +
    scale_size_identity() +
    theme_void() +
    labs(
      title = "LTU adjusted player-pair connection network",
      subtitle = "Edge width = minutes shared | Blue = positive connection score, Red = negative | Node size = total minutes played"
    )
}
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


ltu_minutes <- compute_player_minutes(stints_df_clean, "LTU", player_lookup)
print(ltu_minutes)

network_plot <- build_connection_network_plot(ltu_connection, ltu_minutes)
network_plot
ggsave("LTU_connection_network.png", network_plot, width = 10, height = 8, dpi = 300)


player_connectivity <- summarise_player_connectivity(ltu_connection)
print(player_connectivity)




# Short tables for interpretation in the report.
stable_positive_pairs <- final_pair_table %>%
  filter(stability == "Stable positive") %>%
  slice_max(connection_score, n = 5, with_ties = FALSE)

stable_negative_pairs <- final_pair_table %>%
  filter(stability == "Stable negative") %>%
  slice_min(connection_score, n = 5, with_ties = FALSE)

print(stable_positive_pairs)
print(stable_negative_pairs)

# Save the principal outputs.
write.csv(reconstruction_checks,
          "LTU_reconstruction_checks.csv", row.names = FALSE)
write.csv(ltu_ratings,
          "LTU_rapm_ratings_full_precision.csv", row.names = FALSE)
write.csv(final_pair_table,
          "LTU_final_pair_connection_table.csv", row.names = FALSE)
write.csv(player_connectivity,
          "LTU_player_connectivity_summary.csv", row.names = FALSE)



### interactive network analysis 
# Run once if needed:
# install.packages(c("visNetwork", "htmlwidgets"))

library(dplyr)
library(visNetwork)
library(scales)
library(htmlwidgets)

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
  
  visNetwork(
    nodes = nodes,
    edges = edges,
    width = "100%",
    height = "800px",
    main = "LTU adjusted player-pair connection network"
  ) %>%
    visNodes(
      shape = "dot",
      scaling = list(min = 12, max = 35)
    ) %>%
    visEdges(
      smooth = FALSE,
      scaling = list(min = 1, max = 8)
    ) %>%
    visPhysics(
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
    visEvents(
      stabilizationIterationsDone = "
    function () {
      this.setOptions({physics: false});
      this.fit();
    }
  "
    ) %>%
    visOptions(
      highlightNearest = list(
        enabled = TRUE,
        degree = 1,
        hover = TRUE
      ),
      nodesIdSelection = TRUE
    ) %>%
    visInteraction(
      hover = TRUE,
      navigationButtons = TRUE,
      keyboard = TRUE
    )
}


interactive_network <- build_interactive_connection_network(
  ltu_connection,
  ltu_minutes
)

interactive_network




# 
build_connection_network_plot <- function(ltu_connection, ltu_minutes) {
  library(igraph)
  library(ggraph)
  library(dplyr)
  library(scales)
  library(grid)
  
  edges <- ltu_connection %>%
    rename(
      from = player_1,
      to = player_2,
      conn_score = connection_score
    )
  
  nodes <- ltu_minutes %>%
    rename(name = player_name) %>%
    filter(name %in% union(edges$from, edges$to))
  
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
      aes(label = name),
      repel = TRUE,
      size = 3.4,
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



static_network <- build_connection_network_plot(
  ltu_connection,
  ltu_minutes
)

static_network



## Of the 73 player pairs that shared at least 15 minutes across at least two games, 
## 35 showed stable positive connection scores, 28 showed stable negative scores and 10 were uncertain. 
## Stability was assessed by repeatedly removing one game and recalculating the model. 
## Stable positive pairs therefore maintained an above-expectation association across most available tests, 
## while uncertain pairs were more dependent on particular games. These findings identify combinations for further 
## coaching observation rather than proving player chemistry.


# 1. Build the 2025 coaching-usage network ####

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(scales)


compute_pair_minutes_network <- function(
    stints_df,
    team_name,
    player_lookup,
    min_minutes = 1,
    min_games = 1
) {
  
  pair_minutes <- stints_df %>%
    filter(
      team == team_name,
      duration_sec > 0,
      map_int(lineup, length) == 5
    ) %>%
    mutate(
      pairs = map(
        lineup,
        ~ combn(
          sort(as.character(.x)),
          2,
          simplify = FALSE
        )
      )
    ) %>%
    select(
      fixtureId,
      duration_sec,
      pairs
    ) %>%
    unnest(pairs) %>%
    mutate(
      id_1 = map_chr(pairs, 1),
      id_2 = map_chr(pairs, 2)
    ) %>%
    select(-pairs) %>%
    group_by(id_1, id_2) %>%
    summarise(
      minutes_together = sum(duration_sec) / 60,
      n_games_together = n_distinct(fixtureId),
      .groups = "drop"
    ) %>%
    filter(
      minutes_together >= min_minutes,
      n_games_together >= min_games
    ) %>%
    left_join(
      player_lookup,
      by = c("id_1" = "personId")
    ) %>%
    rename(player_1 = player_name) %>%
    left_join(
      player_lookup,
      by = c("id_2" = "personId")
    ) %>%
    rename(player_2 = player_name) %>%
    filter(
      !is.na(player_1),
      !is.na(player_2)
    ) %>%
    arrange(desc(minutes_together))
  
  pair_minutes
}



calculate_centralisation <- function(
    pair_minutes,
    network_label = "2025"
) {
  
  if (nrow(pair_minutes) == 0) {
    stop("The pair-minutes table contains no qualifying connections.")
  }
  
  # Create edge table
  edge_table <- pair_minutes %>%
    transmute(
      from = as.character(id_1),
      to = as.character(id_2),
      minutes_together
    )
  
  # Create player table
  vertex_table <- bind_rows(
    pair_minutes %>%
      transmute(
        name = as.character(id_1),
        player_name = player_1
      ),
    
    pair_minutes %>%
      transmute(
        name = as.character(id_2),
        player_name = player_2
      )
  ) %>%
    distinct(name, .keep_all = TRUE)
  
  # Construct an undirected shared-minutes network
  network_graph <- graph_from_data_frame(
    d = edge_table,
    directed = FALSE,
    vertices = vertex_table
  )
  
  # Minutes are connection strength
  E(network_graph)$weight <-
    E(network_graph)$minutes_together
  
  # Betweenness and closeness need distance:
  # greater shared minutes = shorter distance
  E(network_graph)$distance <-
    1 / E(network_graph)$minutes_together
  
  # Individual player measures
  player_degree <- degree(
    network_graph,
    mode = "all"
  )
  
  player_strength <- strength(
    network_graph,
    weights = E(network_graph)$weight
  )
  
  player_betweenness <- betweenness(
    network_graph,
    directed = FALSE,
    weights = E(network_graph)$distance,
    normalized = TRUE
  )
  
  player_closeness <- closeness(
    network_graph,
    mode = "all",
    weights = E(network_graph)$distance,
    normalized = TRUE
  )
  
  player_eigenvector <- eigen_centrality(
    network_graph,
    directed = FALSE,
    weights = E(network_graph)$weight
  )$vector
  
  # Add weighted strength to the graph for plotting
  V(network_graph)$weighted_strength <-
    as.numeric(player_strength)
  
  # Player-level table
  player_centrality <- tibble(
    network = as.character(network_label),
    personId = V(network_graph)$name,
    player_name = V(network_graph)$player_name,
    degree = as.numeric(player_degree),
    weighted_strength = as.numeric(player_strength),
    betweenness = as.numeric(player_betweenness),
    closeness = as.numeric(player_closeness),
    eigenvector_centrality =
      as.numeric(player_eigenvector)
  ) %>%
    arrange(desc(weighted_strength))
  
  number_players <- vcount(network_graph)
  total_edge_weight <- sum(E(network_graph)$weight)
  
  # Weighted-strength centralisation
  strength_numerator <- sum(
    max(player_strength) - player_strength
  )
  
  strength_denominator <- if (number_players > 2) {
    (number_players - 2) * total_edge_weight
  } else {
    NA_real_
  }
  
  weighted_strength_centralisation <-
    strength_numerator / strength_denominator
  
  # Betweenness centralisation
  raw_betweenness <- betweenness(
    network_graph,
    directed = FALSE,
    weights = E(network_graph)$distance,
    normalized = FALSE
  )
  
  betweenness_numerator <- sum(
    max(raw_betweenness) - raw_betweenness
  )
  
  betweenness_denominator <- if (number_players > 2) {
    ((number_players - 1)^2 *
       (number_players - 2)) / 2
  } else {
    NA_real_
  }
  
  betweenness_centralisation <-
    betweenness_numerator /
    betweenness_denominator
  
  # Whole-network table
  network_centralisation <- tibble(
    network = as.character(network_label),
    n_players = number_players,
    n_edges = ecount(network_graph),
    network_density = edge_density(
      network_graph,
      loops = FALSE
    ),
    weighted_strength_centralisation =
      weighted_strength_centralisation,
    betweenness_centralisation =
      betweenness_centralisation
  )
  
  list(
    graph = network_graph,
    player_centrality = player_centrality,
    network_centralisation =
      network_centralisation
  )
}
# Create shared-minutes edges for the full 2025 season.
# A low one-minute threshold is used because this is a usage network,
# rather than the stricter adjusted-connection analysis.
pair_minutes_2025 <- compute_pair_minutes_network(
  stints_df = stints_df_clean,
  team_name = "LTU",
  player_lookup = player_lookup,
  min_minutes = 1,
  min_games = 1
)

# Calculate player centrality and whole-team centralisation.
centrality_2025 <- calculate_centralisation(
  pair_minutes = pair_minutes_2025,
  network_label = "2025"
)

# 2. Player centrality ####

player_centrality_2025 <- centrality_2025$player_centrality

player_centrality_2025


#Top players by weighted strength
top_strength <- player_centrality_2025 %>%
  slice_max(
    weighted_strength,
    n = 10,
    with_ties = FALSE
  )

ggplot(
  top_strength,
  aes(
    x = reorder(player_name, weighted_strength),
    y = weighted_strength
  )
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_classic(base_size = 14) +
  labs(
    title = "Most central LTU players by shared minutes",
    subtitle = "2025 season",
    x = "Player",
    y = "Cumulative pair-minutes"
  )


#Top players by betweenness
top_betweenness <- player_centrality_2025 %>%
  slice_max(
    betweenness,
    n = 10,
    with_ties = FALSE
  )

ggplot(
  top_betweenness,
  aes(
    x = reorder(player_name, betweenness),
    y = betweenness
  )
) +
  geom_col(fill = "lightblue3") +
  coord_flip() +
  theme_classic(base_size = 14) +
  labs(
    title = "LTU players by betweenness centrality",
    subtitle = "2025 shared-playing-time network",
    x = "Player",
    y = "Betweenness centrality"
  )

# Top players by eigenvector centrality
top_eigenvector <- player_centrality_2025 %>%
  slice_max(
    eigenvector_centrality,
    n = 10,
    with_ties = FALSE
  )

ggplot(
  top_eigenvector,
  aes(
    x = reorder(player_name, eigenvector_centrality),
    y = eigenvector_centrality
  )
) +
  geom_col(fill = "aquamarine3") +
  coord_flip() +
  theme_classic(base_size = 14) +
  labs(
    title = "LTU players by eigenvector centrality",
    subtitle = "2025 shared-playing-time network",
    x = "Player",
    y = "Eigenvector centrality"
  )



# 3. Revised implementation for LTU #####


library(dplyr)
library(igraph)

# Package-free Gini coefficient
gini_coefficient <- function(x) {
  
  x <- as.numeric(x)
  x <- x[is.finite(x) & !is.na(x)]
  
  if (length(x) == 0 || sum(x) == 0) {
    return(0)
  }
  
  x <- sort(x)
  n <- length(x)
  
  sum(
    (2 * seq_len(n) - n - 1) * x
  ) / (n * sum(x))
}


calculate_centralisation <- function(
    pair_minutes,
    network_label = "2025"
) {
  
  if (nrow(pair_minutes) == 0) {
    stop("The pair-minutes table contains no qualifying connections.")
  }
  
  # Edge table
  edge_table <- pair_minutes %>%
    transmute(
      from = as.character(id_1),
      to = as.character(id_2),
      minutes_together
    )
  
  # Player table
  vertex_table <- bind_rows(
    pair_minutes %>%
      transmute(
        name = as.character(id_1),
        player_name = player_1
      ),
    
    pair_minutes %>%
      transmute(
        name = as.character(id_2),
        player_name = player_2
      )
  ) %>%
    distinct(name, .keep_all = TRUE)
  
  # Undirected shared-minutes network
  network_graph <- graph_from_data_frame(
    d = edge_table,
    directed = FALSE,
    vertices = vertex_table
  )
  
  # Raw minutes represent connection strength
  E(network_graph)$weight <-
    E(network_graph)$minutes_together
  
  # For shortest-path measures:
  # more minutes together = shorter network distance
  E(network_graph)$distance <-
    1 / E(network_graph)$minutes_together
  
  # ----------------------------------------------------------
  # Player-level centrality
  # ----------------------------------------------------------
  
  player_degree <- degree(
    network_graph,
    mode = "all"
  )
  
  player_strength <- strength(
    network_graph,
    weights = E(network_graph)$weight
  )
  
  weighted_betweenness_raw <- betweenness(
    network_graph,
    directed = FALSE,
    weights = E(network_graph)$distance,
    normalized = FALSE
  )
  
  weighted_betweenness_normalised <- betweenness(
    network_graph,
    directed = FALSE,
    weights = E(network_graph)$distance,
    normalized = TRUE
  )
  
  player_closeness <- closeness(
    network_graph,
    mode = "all",
    weights = E(network_graph)$distance,
    normalized = TRUE
  )
  
  player_eigenvector <- eigen_centrality(
    network_graph,
    directed = FALSE,
    weights = E(network_graph)$weight
  )$vector
  
  # Add attributes for network plotting
  V(network_graph)$weighted_strength <-
    as.numeric(player_strength)
  
  V(network_graph)$weighted_betweenness <-
    as.numeric(weighted_betweenness_normalised)
  
  player_centrality <- tibble(
    network = as.character(network_label),
    personId = V(network_graph)$name,
    player_name = V(network_graph)$player_name,
    degree = as.numeric(player_degree),
    weighted_strength = as.numeric(player_strength),
    weighted_betweenness =
      as.numeric(weighted_betweenness_normalised),
    weighted_closeness =
      as.numeric(player_closeness),
    eigenvector_centrality =
      as.numeric(player_eigenvector)
  ) %>%
    arrange(desc(weighted_strength))
  
  # ----------------------------------------------------------
  # Network-level measures
  # ----------------------------------------------------------
  
  n_players <- vcount(network_graph)
  
  strength_mean <- mean(player_strength)
  strength_sd <- sd(player_strength)
  
  # Coefficient of variation:
  # larger value = greater variation in shared-minute exposure
  strength_cv <- if (strength_mean > 0) {
    strength_sd / strength_mean
  } else {
    NA_real_
  }
  
  # Gini:
  # 0 = completely equal;
  # closer to 1 = concentrated among fewer players
  strength_gini <- gini_coefficient(
    player_strength
  )
  
  weighted_betweenness_gini <- gini_coefficient(
    weighted_betweenness_raw
  )
  
  # ----------------------------------------------------------
  # Standard unweighted Freeman betweenness centralisation
  # ----------------------------------------------------------
  # weights = NA deliberately ignores playing-time weights.
  # This makes the Freeman denominator appropriate.
  
  unweighted_betweenness <- betweenness(
    network_graph,
    directed = FALSE,
    weights = NA,
    normalized = FALSE
  )
  
  freeman_numerator <- sum(
    max(unweighted_betweenness) -
      unweighted_betweenness
  )
  
  freeman_denominator <- if (n_players > 2) {
    ((n_players - 1)^2 *
       (n_players - 2)) / 2
  } else {
    NA_real_
  }
  
  freeman_betweenness_centralisation <-
    freeman_numerator /
    freeman_denominator
  
  network_centralisation <- tibble(
    network = as.character(network_label),
    n_players = n_players,
    n_edges = ecount(network_graph),
    
    network_density = edge_density(
      network_graph,
      loops = FALSE
    ),
    
    mean_weighted_strength =
      mean(player_strength),
    
    strength_cv =
      strength_cv,
    
    strength_gini =
      strength_gini,
    
    weighted_betweenness_gini =
      weighted_betweenness_gini,
    
    freeman_betweenness_centralisation =
      freeman_betweenness_centralisation
  )
  
  list(
    graph = network_graph,
    player_centrality = player_centrality,
    network_centralisation =
      network_centralisation
  )
}


# Use the full usage network with the one-minute and one-game thresholds:

pair_minutes_2025 <- compute_pair_minutes_network(
  stints_df = stints_df_clean,
  team_name = "LTU",
  player_lookup = player_lookup,
  min_minutes = 1,
  min_games = 1
)

centrality_2025 <- calculate_centralisation(
  pair_minutes = pair_minutes_2025,
  network_label = "LTU 2025"
)

centrality_2025$network_centralisation %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 3)
    )
  ) %>% as.data.frame() %>% print()


centrality_2025$player_centrality %>%
  select(
    player_name,
    degree,
    weighted_strength,
    weighted_betweenness,
    weighted_closeness,
    eigenvector_centrality
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 3)
    )
  ) %>%
  arrange(desc(weighted_betweenness)) 



#Top bridging players
top_bridging_players <- centrality_2025$player_centrality %>%
  slice_max(
    weighted_betweenness,
    n = 10,
    with_ties = FALSE
  )

ggplot(
  top_bridging_players,
  aes(
    x = reorder(
      player_name,
      weighted_betweenness
    ),
    y = weighted_betweenness
  )
) +
  geom_col(fill = "lightblue3") +
  coord_flip() +
  theme_classic(base_size = 14) +
  labs(
    title = "LTU players by weighted betweenness",
    subtitle = paste(
      "Stronger shared-minute connections",
      "are treated as shorter network distances"
    ),
    x = "Player",
    y = "Weighted betweenness"
  )



all_players <- unique(c(pair_minutes_2025$player_1, pair_minutes_2025$player_2))
length(all_players)  # should be exactly 16

all_possible <- combn(sort(all_players), 2, simplify = FALSE)

existing_pairs <- pair_minutes_2025 %>%
  transmute(a = pmin(player_1, player_2), b = pmax(player_1, player_2))

missing_pairs <- tibble(
  a = map_chr(all_possible, 1),
  b = map_chr(all_possible, 2)
) %>%
  anti_join(existing_pairs, by = c("a", "b"))

nrow(missing_pairs)  # should now be 8, matching 120 - 112
print(missing_pairs)
bind_rows(
  missing_pairs %>% select(player = a),
  missing_pairs %>% select(player = b)
) %>%
  count(player, sort = TRUE)


ltu_minutes %>% filter(player_name %in% c("Coco Erin", "Alana  Steele", "Nicoletta  Karakiklas", "Tahlia Leeson"))



# game-participation check: 
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

game_participation <- compute_player_game_participation(stints_df_clean, "LTU", player_lookup)
print(game_participation)

# specifically check the four flagged low-exposure players
game_participation %>%
  filter(player_name %in% c("Alana  Steele", "Nicoletta  Karakiklas", "Coco Erin", "Anastasia Gak"))


## ============================================================
## SITUATIONAL LINEUP STRATEGY
## Score margin, time remaining, late-game and clutch usage
## ============================================================

enriched_stints <- add_game_state_variables(
  stints_df_clean
)


## ------------------------------------------------------------
## 1. Validation checks
## ------------------------------------------------------------
score_margin_check <- enriched_stints %>%
  filter(team == "LTU") %>%
  summarise(
    min_margin = min(
      score_margin_start,
      na.rm = TRUE
    ),
    max_margin = max(
      score_margin_start,
      na.rm = TRUE
    ),
    missing_margin = sum(
      is.na(score_margin_start)
    )
  )

clutch_coverage <- enriched_stints %>%
  filter(
    team == "LTU",
    clutch_situation
  ) %>%
  summarise(
    n_stints = n(),
    total_minutes = sum(duration_sec) / 60,
    n_games = n_distinct(fixtureId)
  )

score_state_coverage <- enriched_stints %>%
  filter(team == "LTU") %>%
  group_by(score_state) %>%
  summarise(
    n_stints = n(),
    total_minutes = sum(duration_sec) / 60,
    n_games = n_distinct(fixtureId),
    .groups = "drop"
  ) %>%
  arrange(score_state)

print(score_margin_check)
print(clutch_coverage)
print(score_state_coverage)


## ------------------------------------------------------------
## 2. Helper: convert a personId lineup into player names
## ------------------------------------------------------------
make_lineup_label <- function(
    lineup_ids,
    player_lookup
) {
  lineup_ids <- as.character(lineup_ids)
  
  lineup_names <- player_lookup$player_name[
    match(
      lineup_ids,
      player_lookup$personId
    )
  ]
  
  # Retain the ID if a display name is unexpectedly unavailable.
  lineup_names[is.na(lineup_names)] <-
    lineup_ids[is.na(lineup_names)]
  
  paste(
    sort(lineup_names),
    collapse = " | "
  )
}


## ------------------------------------------------------------
## 3. Five-player lineup performance by score state
## ------------------------------------------------------------
situational_lineup_summary <- enriched_stints %>%
  filter(team == "LTU") %>%
  mutate(
    lineup_name = map_chr(
      lineup,
      make_lineup_label,
      player_lookup = player_lookup
    )
  ) %>%
  group_by(
    lineup_name,
    score_state
  ) %>%
  summarise(
    minutes = sum(duration_sec) / 60,
    games = n_distinct(fixtureId),
    n_stints = n(),
    points_for = sum(team_points),
    points_against = sum(opp_points),
    net_points = points_for - points_against,
    net_per_minute = if_else(
      minutes > 0,
      net_points / minutes,
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(
    evidence_level = case_when(
      minutes >= 5 & games >= 2 ~ "Adequate exposure",
      TRUE ~ "Limited exposure"
    )
  ) %>%
  arrange(
    score_state,
    desc(minutes)
  )

# Use this smaller table for the principal interpretation.
lineups_with_adequate_exposure <-
  situational_lineup_summary %>%
  filter(evidence_level == "Adequate exposure")

print(situational_lineup_summary)
print(lineups_with_adequate_exposure)


## ------------------------------------------------------------
## 4. Five-player lineup performance by game phase
## ------------------------------------------------------------
lineup_by_game_phase <- enriched_stints %>%
  filter(team == "LTU") %>%
  mutate(
    lineup_name = map_chr(
      lineup,
      make_lineup_label,
      player_lookup = player_lookup
    )
  ) %>%
  group_by(
    lineup_name,
    game_phase
  ) %>%
  summarise(
    minutes = sum(duration_sec) / 60,
    games = n_distinct(fixtureId),
    n_stints = n(),
    net_points =
      sum(team_points) -
      sum(opp_points),
    net_per_minute = if_else(
      minutes > 0,
      net_points / minutes,
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  arrange(
    game_phase,
    desc(minutes)
  )

print(lineup_by_game_phase)


## ------------------------------------------------------------
## 5. Late-game and clutch lineup summaries
## ------------------------------------------------------------
late_game_lineups <- enriched_stints %>%
  filter(
    team == "LTU",
    late_game
  ) %>%
  mutate(
    lineup_name = map_chr(
      lineup,
      make_lineup_label,
      player_lookup = player_lookup
    )
  ) %>%
  group_by(lineup_name) %>%
  summarise(
    minutes = sum(duration_sec) / 60,
    games = n_distinct(fixtureId),
    n_stints = n(),
    net_points =
      sum(team_points) -
      sum(opp_points),
    net_per_minute = if_else(
      minutes > 0,
      net_points / minutes,
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(minutes))

clutch_lineups <- enriched_stints %>%
  filter(
    team == "LTU",
    clutch_situation
  ) %>%
  mutate(
    lineup_name = map_chr(
      lineup,
      make_lineup_label,
      player_lookup = player_lookup
    )
  ) %>%
  group_by(lineup_name) %>%
  summarise(
    minutes = sum(duration_sec) / 60,
    games = n_distinct(fixtureId),
    n_stints = n(),
    net_points =
      sum(team_points) -
      sum(opp_points),
    net_per_minute = if_else(
      minutes > 0,
      net_points / minutes,
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(minutes))

print(late_game_lineups)
print(clutch_lineups)


## ------------------------------------------------------------
## 6. Pair usage by score state
## ------------------------------------------------------------
pair_by_score_state <- enriched_stints %>%
  filter(team == "LTU") %>%
  mutate(
    pairs = map(
      lineup,
      ~ combn(
        sort(as.character(.x)),
        2,
        simplify = FALSE
      )
    )
  ) %>%
  select(
    fixtureId,
    duration_sec,
    score_state,
    pairs
  ) %>%
  unnest(pairs) %>%
  mutate(
    id_1 = map_chr(pairs, 1),
    id_2 = map_chr(pairs, 2)
  ) %>%
  group_by(
    id_1,
    id_2,
    score_state
  ) %>%
  summarise(
    minutes = sum(duration_sec) / 60,
    games = n_distinct(fixtureId),
    .groups = "drop"
  ) %>%
  complete(
    nesting(id_1, id_2),
    score_state,
    fill = list(
      minutes = 0,
      games = 0
    )
  ) %>%
  pivot_wider(
    names_from = score_state,
    values_from = c(minutes, games),
    names_glue = "{.value}_{score_state}",
    names_expand = TRUE,
    values_fill = 0
  )


## ------------------------------------------------------------
## 7. Combine adjusted connection scores with situational usage
## ------------------------------------------------------------
connection_usage_by_score <- ltu_connection %>%
  left_join(
    pair_by_score_state,
    by = c("id_1", "id_2")
  ) %>%
  mutate(
    across(
      starts_with("minutes_"),
      ~ replace_na(.x, 0)
    ),
    across(
      starts_with("games_"),
      ~ replace_na(.x, 0)
    )
  ) %>%
  arrange(desc(connection_score)) %>%
  select(
    player_1,
    player_2,
    connection_score,
    minutes_together,
    n_games_together,
    starts_with("minutes_"),
    starts_with("games_")
  )

print(connection_usage_by_score)


## ------------------------------------------------------------
## 8. Check whether timeout events are available
## ------------------------------------------------------------
# This only confirms how timeouts are coded. A separate event-to-stint
# matching step is required before estimating post-timeout performance.
timeout_events <- LTU_pbp %>%
  filter(
    str_detect(
      str_to_lower(
        paste(
          action,
          detail
        )
      ),
      "timeout|time out"
    )
  ) %>%
  select(
    fixtureId,
    period,
    clock_seconds,
    team,
    action,
    detail
  ) %>%
  arrange(
    fixtureId,
    period,
    desc(clock_seconds)
  )

print(timeout_events)


## ------------------------------------------------------------
## 9. Save situational-strategy outputs
## ------------------------------------------------------------
write.csv(
  score_state_coverage,
  "LTU_score_state_coverage.csv",
  row.names = FALSE
)

write.csv(
  situational_lineup_summary,
  "LTU_situational_lineup_summary.csv",
  row.names = FALSE
)

write.csv(
  lineup_by_game_phase,
  "LTU_lineup_by_game_phase.csv",
  row.names = FALSE
)

write.csv(
  late_game_lineups,
  "LTU_late_game_lineups.csv",
  row.names = FALSE
)

write.csv(
  clutch_lineups,
  "LTU_clutch_lineups.csv",
  row.names = FALSE
)

write.csv(
  connection_usage_by_score,
  "LTU_connection_usage_by_score.csv",
  row.names = FALSE
)

