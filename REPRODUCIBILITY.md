# Reproduce the LTU analysis

## Install this update
Copy R/LTU-team.R, R/LTU-league-comparison.R, run-analysis.R and save-analysis-inputs.R into the matching locations in Basketball-Project-cleaned. Keep your current prediction and per-game scripts. Merge the supplied .gitignore rules with any other rules you need.

## Preserve the final inputs first
In the existing R session that produced the final results, before restarting R, run:

```r
source("save-analysis-inputs.R")
```

This requires fixtures_df, LTU_pbp and all_results_men from the SAME completed analysis. It does not use the old raw files, which may cover only a subset of games. saved_at is the export date, not a claimed original download date.

If that session is gone, a new download cannot guarantee recovery of the old inputs. Instead create a new documented analysis baseline using the optional refresh below, and update the report if results differ.

## Optional new data baseline
Back up any existing input snapshots first: this deliberately overwrites both input files. Run only when you want new data, not to reproduce a submitted run:

```r
options(ltu.refresh_inputs = TRUE)
tryCatch(source("run-analysis.R"),
         finally = options(ltu.refresh_inputs = FALSE))
```

A failed refresh can leave mixed old/new files. Do not publish that set: complete the refresh successfully and verify both snapshots. Fresh inputs can change results.

## Test the saved-input workflow
Restart R, open Basketball-Project.Rproj, and run:

```r
options(ltu.refresh_inputs = FALSE)
source("run-analysis.R")
```

The team and league scripts should report that saved inputs were loaded. Repeat with the network disconnected after packages are installed to confirm the workflow does not depend on the feed. Compare reconstruction counts, pair results and prediction metrics with the final report. Tiny floating-point differences can occur across environments.

The seed is fixed at runner entry. Package versions still matter; preserve renv.lock and the renv bootstrap files if you use renv. session-info.txt documents the actual run environment but is not a dependency installer.

## What to commit
After checking permission to share the inputs and reviewing their contents, commit the revised scripts, this guide, .gitignore and the two explicitly allowed RDS input snapshots. Commit regenerated outputs only after validation. Do not commit private reports or credentials. A fresh clone must contain both inputs and all four analysis scripts.

Update data/DATA.md with source details, snapshot provenance, collection dates if known, and limitations. In README.md replace the live-download description with: "The analysis reads fixed input snapshots from data/raw/ by default. Refreshing the source data is optional and may change the results. See REPRODUCIBILITY.md."

The private report remains outside Git. To reproduce that document, the assessor also needs its sanitised source and assets through an appropriate submission channel. This update addresses the analysis pipeline; it does not establish that an unseen report renders.

## Validation of this update
The replacement team file is based on the supplied November 1 text. Only input acquisition and its introductory comment are changed; downstream calculations are retained. The league file uses the previously supplied cleaned version with a saved-input branch. The existing runner gains a seed and corrected comments. No R runtime or input data was available here, so execution and numerical equality must be checked locally before claiming full reproducibility.
