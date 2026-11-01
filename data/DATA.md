# Data documentation

This project uses data from the 2025 University Basketball League (UBL) to study player connections, rotation structure and situational deployment in La Trobe University’s women’s team (LTU). Women’s and men’s league results provide a separate team-level comparison.

## Data source and coverage

Fixture results and play-by-play records are retrieved from the Sportradar embed feed used by UniSport. The API base used in the scripts is:

```text
https://embed-api.eui.connect.sportradar.com/v1/embed/303
```

The analysis now loads saved input snapshots by default.

`R/LTU-team.R` reads the saved women’s fixtures and LTU play-by-play records. `R/LTU-league-comparison.R` uses those women’s fixtures and reads the saved men’s results. Downloading updated inputs is optional; the fixture URLs and state parameters remain in the scripts.

Snapshot export times and provenance are stored within the input files. An export time records when an object was saved and should not be interpreted as its original download time.

The analysis retained eight of ten LTU games after excluding games with less than 50% reconstructed playing-time coverage. The two excluded games had starting-lineup detection failures. Of the eight retained games, six had complete 40-minute coverage and matched the play-by-play scoring totals. The other two had 79.0% and 84.5% coverage, with missing time and scoring events. These gaps may affect the estimated player and pair scores. Complete coverage and matching scores do not independently verify lineup identities. See outputs/tables/LTU_reconstruction_checks.csv for game-level coverage and scoring checks.

## Files in `data/raw/`


The analysis uses these two input snapshots. These snapshots contain extracted tables rather than untouched API responses.

| File | Contents | Role |
| --- | --- | --- |
| `LTU_women_inputs_2025.rds` | A list containing `fixtures_df`, `LTU_pbp`, export time and provenance | Women’s fixtures and LTU event inputs used by the team analysis |
| `UBL_men_results_2025.rds` | A list containing `results`, export time and provenance | Men’s results used by the league comparison |

Inspect the saved inputs from the project root with:

```r
women_inputs <- readRDS("data/raw/LTU_women_inputs_2025.rds")
names(women_inputs)
str(women_inputs$LTU_pbp)

men_inputs <- readRDS("data/raw/UBL_men_results_2025.rds")
str(men_inputs$results)
```

`save-analysis-inputs.R` preserves these inputs from a completed analysis session. It is a preparation step and is not required for routine reruns.



## Main fields

### Play-by-play records

| Field | Meaning |
| --- | --- |
| `fixtureId` | Game identifier |
| `round`, `startTimeLocal` | Fixture round and source-provided local start time |
| `matchup`, `team` | Matchup label and team associated with the event |
| `period`, `clock_seconds` | Period number and seconds remaining in that period |
| `personId`, `player` | Player identifier and display name |
| `action`, `detail` | Event type and subtype, including substitution direction |
| `success` | Recorded success indicator for the event |
| `eventId` | Event identifier |
| `x`, `y`, `bib` | Source-provided coordinates and player bib information, where available |

Player IDs are used to identify players in the modelling pipeline. Names are joined to make outputs readable; whitespace is cleaned for presentation labels.

### Reconstructed stints

A stint is an interval with an unchanged inferred lineup. 
The stint table contains rows from both teams’ perspectives, so its total row count should not be reported as the number of LTU stints without filtering `team == "LTU"`.

| Field | Meaning |
| --- | --- |
| `lineup`, `opp_lineup` | Lists of inferred on-court player IDs |
| `opponent` | Opponent team |
| `duration_sec` | Stint duration in seconds |
| `team_points`, `opp_points` | Points scored and conceded during the stint |
| `start_clock_seconds`, `end_clock_seconds` | Period clock at the start and end |
| `score_margin_start` | Team score minus opponent score at stint start |

The model response is net points per minute: `(team_points - opp_points) / (duration_sec / 60)`.

## Preparation and quality checks

The scripts order events by period and game clock, infer opening lineups from activity before the first substitution, and update lineups using substitutions. Lineups are carried across period boundaries. Inconsistent activity at a new period is flagged, but not automatically corrected.

Games below 50% reconstructed coverage are excluded using an assumed 40-minute regulation game. Regression and connection scoring exclude stints shorter than 15 seconds. Pair scores require at least 15 shared model-eligible minutes across two games. The separate usage network requires one shared minute in one game. The per-game network uses a two-minute display threshold.

Quality checks compare reconstructed scores with scoring events from the same feed. Agreement does not independently verify player identities or official final scores. Quarter-break discrepancy counts are printed to the analysis console and should be retained with the analysis log. Situational categories are assigned at stint start; intervals crossing score or clock thresholds are not split.

## Derived tables and figures

Derived CSV tables are written to `outputs/tables/` and plots to `Figures/`.

| Script | Main saved tables |
| --- | --- |
| `R/LTU-team.R` | Reconstruction checks, RAPM ratings, final pair scores, player connectivity, centrality, game participation and situational summaries |
| `R/LTU-prediction.R` | Stint predictions, per-game and overall errors, permutation summary |
| `R/LTU-network-by-game.R` | Pair scores by game and game-label lookup |
| `R/LTU-league-comparison.R` | Women’s and men’s eigenvector scores and league comparison |

Connection scores are duration-weighted averages of full-lineup model residuals. Positive scores indicate above-prediction performance in shared stints, not proof of causal chemistry. Per-game connection scores use the season model and are descriptive; held-out predictions are calculated separately.

## Report snapshot

`LTU_report_data.rds` is saved in the project root after all four stages finish successfully. It contains the play-by-play table, reconstructed stints, lookup, model, pair and centrality outputs, predictions, permutation gains, preparation time and session information.

```r
report_data <- readRDS("LTU_report_data.rds")
names(report_data)
report_data$prepared_at
```

The snapshot supports report rendering without another download when the report reads it. It does not contain every league-comparison object; the presentation also uses saved CSVs and figures. `session-info.txt` records the R and loaded package versions from the completed run.

This is a derived output snapshot. Reproducing the analysis starts from
the two input files in `data/raw/`, rather than from this report snapshot.

## Reproducing the outputs

Open `Basketball-Project.Rproj`, restart R, and run the following from
the project root:

```r
options(ltu.refresh_inputs = FALSE)
source("run-analysis.R")
```

Install the required packages listed in `README.md` before running the analysis. Once the packages and saved inputs are available, the analysis can run without accessing the live feed.

The runner executes the team, prediction, per-game network and league-comparison scripts in that order. It sets a random seed, then saves the report snapshot and session information after successful completion. 

Downloading updated data is optional, requires internet access and may change the results. See `REPRODUCIBILITY.md` for refresh instructions. Preserve the submitted input snapshots before refreshing them.

R versions, package versions and platforms may cause small numerical differences. `session-info.txt` records the environment used for the completed run; it does not automatically install those package versions.

Rendering the report or presentation also requires its source, referenced assets and rendering software. Running the analysis alone does not render those documents.

## Data use and identification

Source and derived tables contain player names and identifiers. Presentation figures use anonymous display labels, but this does not anonymise the underlying data or make the mapping irreversible. Review data-sharing permissions before distributing source files or named outputs.

The repository’s MIT licence covers the project’s own code. It does not grant rights to third-party basketball data, university logos or separately licensed materials. Keep source attribution and describe any access restrictions in the repository README.
