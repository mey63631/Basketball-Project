# LTU connection scores: held-out prediction
# Run after source("R/LTU-team.R"), from the project root.
# Each game is tested using a model and pair scores fitted without that game.

# 1. Packages and inputs ----

library(glmnet)
library(purrr)
library(dplyr)

required_objects <- c(
  "stints_df_clean", "player_lookup", "compute_connection_scores",
  "ltu_settings", "write_ltu_table"
)
missing_objects <- required_objects[!vapply(
  required_objects, exists, logical(1), inherits = TRUE
)]
if (length(missing_objects)) {
  stop("Run R/LTU-team.R first. Missing: ", paste(missing_objects, collapse = ", "))
}

prediction_settings <- list(
  min_pair_minutes = ltu_settings$min_pair_minutes,
  min_pair_games = ltu_settings$min_pair_games,
  min_stint_seconds = 15,
  permutations = 2000,
  seed = 1
)

# 2. Match test lineups to the training model ----

# A test player or opponent not seen in training has no fitted coefficient.
# Its indicator is left at zero, matching the original prediction procedure.
make_newx <- function(test_stints, train_cols) {
  X <- matrix(
    0, nrow = nrow(test_stints), ncol = length(train_cols),
    dimnames = list(NULL, train_cols)
  )
  for (i in seq_len(nrow(test_stints))) {
    ids <- intersect(as.character(test_stints$lineup[[i]]), train_cols)
    X[i, ids] <- 1
    opp_col <- paste0("opponent", test_stints$opponent[i])
    if (opp_col %in% train_cols) X[i, opp_col] <- 1
  }
  X
}

# Five players give ten pairs. Pairs without a training score contribute zero.
# The mean is a fixed adjustment, not a separately fitted interaction model.
lineup_pair_feature <- function(lineup_ids, score_lookup) {
  prs <- combn(sort(as.character(lineup_ids)), 2, simplify = FALSE)
  keys <- map_chr(prs, ~ paste(.x, collapse = "|"))
  vals <- score_lookup[keys]
  vals[is.na(vals)] <- 0
  mean(vals)
}

# 3. Leave one game out and predict it ----

evaluate_connection_prediction <- function(
    stints_df, team_name, player_lookup,
    min_minutes = 15, min_games = 2, min_duration_sec = 15) {
  # The supplied model builder uses a fixed 15-second training threshold.
  if (min_duration_sec != 15) {
    stop("Training and testing currently require the original 15-second threshold.")
  }
  game_ids <- unique(stints_df$fixtureId[stints_df$team == team_name])

  map_dfr(game_ids, function(g) {
    train <- stints_df %>% filter(fixtureId != g)
    test <- stints_df %>% filter(
      fixtureId == g, team == team_name, duration_sec >= min_duration_sec
    )
    if (nrow(test) == 0) stop("No eligible test stints for fixture ", g)
    if (any(map_int(test$lineup, length) != 5)) {
      stop("Incomplete test lineup in fixture ", g)
    }

    # Stop on a failed fold instead of silently reporting fewer games.
    scores <- tryCatch(
      compute_connection_scores(
        train, team_name, player_lookup,
        min_minutes = min_minutes, min_games = min_games,
        lambda_choice = ltu_settings$lambda_choice
      ),
      error = function(e) {
        stop("Prediction failed for fixture ", g, ": ", conditionMessage(e),
             call. = FALSE)
      }
    )
    model <- attr(scores, "rapm_model")
    if (is.null(model)) stop("No fitted model for fixture ", g)
    cvfit <- model$cvfit
    train_cols <- rownames(coef(cvfit, s = model$lambda_choice))[-1]
    newx <- make_newx(test, train_cols)
    base_pred <- as.numeric(predict(cvfit, newx = newx, s = model$lambda_choice))

    score_lookup <- setNames(
      scores$connection_score, paste(scores$id_1, scores$id_2, sep = "|")
    )
    feature <- map_dbl(test$lineup, ~ lineup_pair_feature(.x, score_lookup))

    tibble(
      fixtureId = test$fixtureId,
      duration_sec = test$duration_sec,
      actual = (test$team_points - test$opp_points) / (test$duration_sec / 60),
      base_pred = base_pred,
      pair_feature = feature,
      enhanced_pred = base_pred + feature
    )
  })
}

# 4. Calculate prediction errors ----

wrmse <- function(err, w) sqrt(weighted.mean(err^2, w))
wmae <- function(err, w) weighted.mean(abs(err), w)

pred_df <- evaluate_connection_prediction(
  stints_df_clean, ltu_settings$team, player_lookup,
  min_minutes = prediction_settings$min_pair_minutes,
  min_games = prediction_settings$min_pair_games,
  min_duration_sec = prediction_settings$min_stint_seconds
)
if (nrow(pred_df) == 0 || any(!is.finite(as.matrix(
  pred_df %>% select(duration_sec, actual, base_pred, pair_feature, enhanced_pred)
)))) {
  stop("Prediction output is empty or contains non-finite values.")
}

# Check every eligible test stint was evaluated.
expected_counts <- stints_df_clean %>%
  filter(team == ltu_settings$team,
         duration_sec >= prediction_settings$min_stint_seconds) %>%
  count(fixtureId, name = "expected_stints")
prediction_coverage <- pred_df %>% count(fixtureId, name = "evaluated_stints") %>%
  full_join(expected_counts, by = "fixtureId")
if (anyNA(prediction_coverage) ||
    any(prediction_coverage$expected_stints != prediction_coverage$evaluated_stints)) {
  stop("Some eligible test stints were not evaluated. Inspect prediction_coverage.")
}

overall <- pred_df %>% summarise(
  n_stints = n(),
  rmse_baseline = wrmse(actual - base_pred, duration_sec),
  rmse_enhanced = wrmse(actual - enhanced_pred, duration_sec)
)
per_game <- pred_df %>% group_by(fixtureId) %>% summarise(
  n_stints = n(),
  rmse_baseline = wrmse(actual - base_pred, duration_sec),
  rmse_enhanced = wrmse(actual - enhanced_pred, duration_sec),
  improved = rmse_enhanced < rmse_baseline,
  .groups = "drop"
)
print(overall)
print(per_game)

# 5. Compare the observed gain with shuffled features ----

# Positive gain means the pair adjustment reduced error.
# Pooled shuffling ignores within-game dependence, so this is exploratory.
# Keep the original empirical tail calculation for consistency with earlier results.
set.seed(prediction_settings$seed)
obs_gain <- overall$rmse_baseline - overall$rmse_enhanced
perm_gain <- replicate(prediction_settings$permutations, {
  shuffled <- sample(pred_df$pair_feature)
  overall$rmse_baseline - wrmse(
    pred_df$actual - (pred_df$base_pred + shuffled), pred_df$duration_sec
  )
})
permutation_p <- mean(perm_gain >= obs_gain)
p_value <- permutation_p
permutation_summary <- tibble(
  observed_gain = obs_gain,
  permutation_p = permutation_p,
  n_permutations = prediction_settings$permutations
)
print(permutation_summary)

# 6. Save prediction outputs ----

write_ltu_table(per_game, "LTU_prediction_per_game.csv")
write_ltu_table(overall, "LTU_prediction_overall.csv")
write_ltu_table(pred_df, "LTU_prediction_stints.csv")
write_ltu_table(permutation_summary, "LTU_prediction_permutation.csv")
# League tables are saved by LTU-league-comparison.R.
# The season network is saved by LTU-team.R.
