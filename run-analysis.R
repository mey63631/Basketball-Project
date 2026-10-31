# Run the LTU basketball analysis ----
# Open Basketball_Project.Rproj, then run source("run-analysis.R").
# The scripts expect paths relative to the project root, not the R/ folder.
# Team and league scripts download live data; this may change later results.

scripts <- c(
  "R/LTU-team.R",
  "R/LTU-prediction.R",
  "R/LTU-network-by-game.R",
  "R/LTU-league-comparison.R"
)
if (!all(file.exists(scripts))) {
  stop("Run this file from the project root and check the four script names.")
}

# Check syntax before starting any downloads.
invisible(lapply(scripts, function(script) parse(file = script)))

# Check core packages before starting the downloads.
packages <- c(
  "httr", "jsonlite", "stringr", "purrr", "tidyr", "glmnet",
  "igraph", "ggraph", "ggplot2", "scales", "dplyr"
)
missing_packages <- packages[!vapply(
  packages, requireNamespace, logical(1), quietly = TRUE
)]
if (length(missing_packages)) {
  stop("Install these packages first: ", paste(missing_packages, collapse = ", "))
}

for (script in scripts) {
  message("Running ", script)
  source(script, local = .GlobalEnv)
}

# Save the model outputs used by LTU_report_with_code.qmd.
# This lets the report render without downloading the feed again.
report_objects <- c(
  "stints_df", "stints_df_clean", "LTU_pbp", "player_lookup",
  "reconstruction_checks", "final_pair_table", "ltu_ratings", "ltu_connection",
  "ltu_minutes", "centrality_2025", "enriched_stints", "pred_df", "perm_gain",
  "season_model"
)
report_data <- mget(report_objects, envir = .GlobalEnv)
report_data$prepared_at <- as.character(Sys.time())
report_data$session_info <- capture.output(sessionInfo())
saveRDS(report_data, "LTU_report_data.rds")

writeLines(capture.output(sessionInfo()), "session-info.txt")
message("Analysis finished. Report snapshot: LTU_report_data.rds")
