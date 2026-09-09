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
    pre_sub <- team_events %>% slice(1:(first_sub_row$row_num - 1))
  } else {
    pre_sub <- team_events
  }
  starters <- unique(pre_sub$personId)
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
    team_events %>% slice(1:(first_sub_row$row_num - 1))
  } else {
    team_events
  }
  actors <- unique(pre_sub$personId)
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
## 2. Build stints (keyed on personId; validates quarter carryover)
## ------------------------------------------------------------
build_stints <- function(pbp_df, period_length = PERIOD_LENGTH_SECONDS, half_boundary_periods = c()) {
  # NOTE: half_boundary_periods defaults to none (pure carryover for
  # every period). An earlier version tried re-detecting starters fresh
  # at period 3 (halftime) using the same heuristic as period 1. That
  # made things WORSE: stint count dropped from 702 to 504 (-28%)
  # because the period-1 heuristic doesn't transfer to halftime — the
  # second half often opens with a substitution almost immediately,
  # so far fewer than 5 players get detected before it, and those
  # stints get silently dropped. Carrying over the lineup and
  # documenting the resulting ~18 flagged discrepancies as a stated
  # limitation preserves far more usable data than trying to "fix"
  # them via re-detection.
  all_stints <- list()
  n_quarter_flags <- 0
  
  # preserve original scrape order as a tie-breaker for events sharing
  # the same clock_seconds (e.g. a shot and a substitution logged at
  # the same second) — arrange() alone doesn't guarantee this
  pbp_df <- pbp_df %>% mutate(.orig_order = row_number())
  
  fixture_ids <- unique(pbp_df$fixtureId)
  
  for (fid in fixture_ids) {
    game_df <- pbp_df %>%
      filter(fixtureId == fid) %>%
      arrange(period, desc(clock_seconds), .orig_order) %>%
      mutate(is_sub = action == "substitution")
    
    teams <- unique(na.omit(game_df$team))
    if (length(teams) != 2) next
    team_a <- teams[1]; team_b <- teams[2]
    
    first_period <- min(game_df$period)
    period1_df <- game_df %>% filter(period == first_period)
    lineup <- list()
    lineup[[team_a]] <- get_starting_lineup(period1_df, team_a)
    lineup[[team_b]] <- get_starting_lineup(period1_df, team_b)
    
    points <- setNames(c(0, 0), c(team_a, team_b))
    current_period <- first_period
    stint_start_clock <- period_length
    n <- nrow(game_df)
    
    flush <- function(end_clock, per) {
      for (pair in list(c(team_a, team_b), c(team_b, team_a))) {
        t <- pair[1]; opp <- pair[2]
        if (length(lineup[[t]]) == 5) {
          own_roster <- sort(lineup[[t]])
          opp_roster <- sort(lineup[[opp]])
          all_stints[[length(all_stints) + 1]] <<- tibble(
            fixtureId = fid, period = per, team = t,
            lineup = list(own_roster), opponent = opp,
            opp_lineup = list(opp_roster),
            team_points = points[[t]], opp_points = points[[opp]],
            duration_sec = stint_start_clock - end_clock
          )
        }
      }
    }
    
    i <- 1
    while (i <= n) {
      row <- game_df[i, ]
      
      if (row$period != current_period) {
        flush(0, current_period)
        points <- setNames(c(0, 0), c(team_a, team_b))
        current_period <- row$period
        stint_start_clock <- period_length
        
        new_period_df <- game_df %>% filter(period == current_period)
        
        if (current_period %in% half_boundary_periods) {
          # halftime break: re-detect starters fresh (same heuristic as
          # period 1) rather than carrying over — evidence from
          # validate_period_start() showed flags cluster overwhelmingly
          # at this boundary (8/18 in period 3 alone), consistent with
          # real halftime lineup changes the substitution log doesn't
          # capture the same way as an in-game sub
          lineup[[team_a]] <- get_starting_lineup(new_period_df, team_a)
          lineup[[team_b]] <- get_starting_lineup(new_period_df, team_b)
        } else {
          # ordinary quarter break: validate the carryover assumption
          n_quarter_flags <- n_quarter_flags +
            validate_period_start(new_period_df, team_a, lineup[[team_a]], fid, current_period) +
            validate_period_start(new_period_df, team_b, lineup[[team_b]], fid, current_period)
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
          if (r$detail == "out") { lineup[[tm]] <- setdiff(lineup[[tm]], r$personId) }
          else if (r$detail == "in") { lineup[[tm]] <- union(lineup[[tm]], r$personId) }
        }
        
        points <- setNames(c(0, 0), c(team_a, team_b))
        stint_start_clock <- row$clock_seconds
        i <- j + 1
        next
      }
      
      if (row$action %in% names(SCORE_POINTS) && isTRUE(row$success)) {
        points[[row$team]] <- points[[row$team]] + SCORE_POINTS[[row$action]]
      }
      
      i <- i + 1
    }
    
    flush(0, current_period)
  }
  
  cat("\nTotal quarter-break lineup discrepancy flags across dataset:", n_quarter_flags,
      "- document this count as a data limitation in your write-up.\n")
  
  bind_rows(all_stints)
}


## ------------------------------------------------------------
## 3. Naive pair performance (keyed on personId, joined to names at the end)
## ------------------------------------------------------------
naive_pair_synergy <- function(stints_df, team_name, player_lookup) {
  team_stints <- stints_df %>% filter(team == team_name)
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
build_player_design_matrix <- function(stints_df, team_name, min_duration_sec = 15) {
  team_stints <- stints_df %>%
    filter(team == team_name, duration_sec >= min_duration_sec) %>%
    mutate(stint_id = row_number())
  
  all_players <- team_stints$lineup %>% unlist(use.names = FALSE) %>% unique() %>% sort()
  
  player_matrix <- matrix(
    0, nrow = nrow(team_stints), ncol = length(all_players),
    dimnames = list(NULL, all_players)
  )
  for (i in seq_len(nrow(team_stints))) {
    players_on <- team_stints$lineup[[i]]
    player_matrix[i, players_on] <- 1
  }
  
  opp_dummies <- model.matrix(~ opponent - 1, data = team_stints)
  X <- cbind(player_matrix, opp_dummies)
  y <- (team_stints$team_points - team_stints$opp_points) / (team_stints$duration_sec / 60)
  w <- team_stints$duration_sec
  
  list(X = X, y = y, w = w, players = all_players, stints = team_stints)
}

#' Ridge regression with folds assigned BY GAME (fixtureId), not
#' randomly across stints — stints from the same game are correlated,
#' so random folding leaks information between train/validation sets.
#' With 10 games this is approximately leave-one-game-out CV.
fit_rapm <- function(stints_df, team_name, player_lookup) {
  dm <- build_player_design_matrix(stints_df, team_name)
  if (nrow(dm$X) < 15) {
    cat("WARNING:", team_name, "has only", nrow(dm$X),
        "stints after filtering - ridge regression will be unstable.\n")
  }
  
  game_ids <- unique(dm$stints$fixtureId)
  fold_lookup <- setNames(seq_along(game_ids), game_ids)
  fold_id <- unname(fold_lookup[dm$stints$fixtureId])
  
  cvfit <- cv.glmnet(dm$X, dm$y, alpha = 0, weights = dm$w, foldid = fold_id)
  coefs <- coef(cvfit, s = "lambda.min")
  player_coefs <- coefs[dm$players, 1]
  
  tibble(personId = dm$players, rapm_rating = as.numeric(player_coefs)) %>%
    left_join(player_lookup, by = "personId") %>%
    select(player_name, personId, rapm_rating) %>%
    arrange(desc(rapm_rating))
}


## ------------------------------------------------------------
## 5. PRIMARY OUTPUT: sequential connection score
## ------------------------------------------------------------
#' Reviewer-recommended primary measure. Actual pair net rating minus
#' the sum of each player's individual RAPM rating = connection_score.
#' Framed explicitly as an association, not causal proof of chemistry.
#' min_minutes raised to 15 per reviewer guidance (was 5) — a pair
#' surviving a handful of possessions is not a reliable signal.
compute_connection_scores <- function(stints_df, team_name, player_lookup, min_minutes = 15) {
  pairs <- naive_pair_synergy(stints_df, team_name, player_lookup) %>%
    filter(minutes_together >= min_minutes), n_games_together >= 2)
  ratings <- fit_rapm(stints_df, team_name, player_lookup)
  rating_lookup <- setNames(ratings$rapm_rating, ratings$personId)
  
  pairs %>%
    mutate(
      rating_1 = rating_lookup[id_1],
      rating_2 = rating_lookup[id_2],
      expected_net_per_minute = rating_1 + rating_2,
      connection_score = net_per_minute - expected_net_per_minute
    ) %>%
    select(player_1, player_2, minutes_together, n_games_together,
           net_per_minute, expected_net_per_minute, connection_score) %>%
    arrange(desc(connection_score))
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
## USAGE
## ------------------------------------------------------------
player_lookup <- build_player_lookup(LTU_pbp)
stints_df     <- build_stints(LTU_pbp)
##
## ltu_pairs      <- naive_pair_synergy(stints_df, "LTU", player_lookup)
## ltu_ratings    <- fit_rapm(stints_df, "LTU", player_lookup)
## ltu_connection <- compute_connection_scores(stints_df, "LTU", player_lookup)
##

stints_df_clean <- exclude_broken_games(stints_df, "LTU", min_pct = 0.5)
nrow(stints_df_clean)  # should be roughly 638 minus the 4 stints from the 2 broken games

ltu_pairs      <- naive_pair_synergy(stints_df_clean, "LTU", player_lookup)
ltu_ratings    <- fit_rapm(stints_df_clean, "LTU", player_lookup)
ltu_connection <- compute_connection_scores(stints_df_clean, "LTU", player_lookup)

print(ltu_connection)

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

## An initially prominent pairing (Hii + Holland) did not survive stricter exposure thresholds and game-quality filtering, illustrating why the higher bar was necessary.


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
      avg_connection_score = round(mean(connection_score), 2),
      total_shared_minutes = round(sum(minutes_together), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(avg_connection_score))
}


ltu_minutes <- compute_player_minutes(stints_df_clean, "LTU", player_lookup)
print(ltu_minutes)

network_plot <- build_connection_network_plot(ltu_connection, ltu_minutes)
network_plot
ggsave("LTU_connection_network.png", network_plot, width = 10, height = 8, dpi = 300)


player_connectivity <- summarise_player_connectivity(ltu_connection)
print(player_connectivity)



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
