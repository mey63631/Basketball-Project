# 01-scrape-data.R
# Scrapes play-by-play data for LTU vs UTAS matches from UniSport (Sportradar embed API)

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)

source("R/scrape_pbp.R")

# Fetch the full season's fixtures list
url <- "https://embed-api.eui.connect.sportradar.com/v1/embed/303/fixtures?state=eJyrVipWslJQMjMwTjQ3T7bUNbM0sNA1NEwz0LVMSjbVtUwxTDM1N0hKSjQxVNJRUMoBKU7N03X1U6oFAK4sDo8"

res <- GET(url, add_headers("User-Agent" = "Mozilla/5.0"))
status_code(res)   # should be 200

fixtures_data <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)

# Build a clean fixtures table
fixtures_df <- fixtures_data$data$fixtures %>%
  mutate(
    state = str_extract(link, "(?<=~w=f~).+"),
    team1 = map_chr(competitors, ~ .x$name[1]),
    team2 = map_chr(competitors, ~ .x$name[2])
  )

head(fixtures_df[, c("team1", "team2", "round")])

# Filter to the matches you want
match_pick <- fixtures_df %>%
  filter((team1 == "LTU" & team2 == "UTAS") |
           (team1 == "UTAS" & team2 == "LTU"))

match_pick[, c("team1", "team2", "round", "startTimeLocal")]

# Pull play-by-play for the filtered matches
filtered_pbp <- lapply(match_pick$state, function(s) {
  Sys.sleep(0.5)
  get_match_pbp(s)
}) %>% bind_rows()

# Tag with round/date
filtered_pbp <- filtered_pbp %>%
  left_join(match_pick %>% select(fixtureId, round, startTimeLocal), by = "fixtureId")

nrow(filtered_pbp)
head(filtered_pbp, 20)

# --- Save output so you don't have to re-scrape every time ---
saveRDS(filtered_pbp, "data/raw/pbp_LTU_UTAS.rds")
saveRDS(fixtures_df, "data/raw/fixtures_season.rds")