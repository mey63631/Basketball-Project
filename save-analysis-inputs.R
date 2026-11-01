# Run ONCE from the project root in the session that produced your final results.
# This saves existing inputs; it does not download or reconstruct anything.
required <- c("fixtures_df", "LTU_pbp", "all_results_men")
missing <- required[!vapply(required, exists, logical(1),
                          envir = .GlobalEnv, inherits = FALSE)]
if (length(missing)) stop("Missing objects: ", paste(missing, collapse = ", "),
    ". Use the completed analysis session; do not substitute unrelated snapshots.")
paths <- file.path("data", "raw", c("LTU_women_inputs_2025.rds", "UBL_men_results_2025.rds"))
if (any(file.exists(paths))) stop("Input snapshots already exist. Preserve them before replacing them.")
if (!all(vapply(mget(required, envir = .GlobalEnv), function(x)
    is.data.frame(x) && nrow(x) > 0, logical(1)))) stop("Inputs must be non-empty data frames.")
dir.create(file.path("data", "raw"), recursive = TRUE, showWarnings = FALSE)
saveRDS(list(fixtures_df = fixtures_df, LTU_pbp = LTU_pbp,
             saved_at = as.character(Sys.time()),
             provenance = "Saved from completed analysis session; original retrieval date not inferred"), paths[1])
saveRDS(list(results = all_results_men, saved_at = as.character(Sys.time()),
             provenance = "Saved from completed analysis session; original retrieval date not inferred"), paths[2])
print(tools::md5sum(paths))
message("Inputs saved. Keep these snapshots fixed for the final submission.")
