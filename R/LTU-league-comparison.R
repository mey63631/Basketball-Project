# UBL 2025: women's and men's league comparison
# Run after R/LTU-team.R. This uses its women's fixtures and Gini helper.
# Scores describe a directed win-margin network, not official league positions.

# 1. Packages, inputs and settings ----

library(httr)
library(jsonlite)
library(stringr)
library(purrr)
library(tidyr)
library(igraph)
library(ggplot2)
library(dplyr)

if (!exists("fixtures_df") || !exists("gini_coefficient") ||
    !exists("write_ltu_table")) {
  stop("Run R/LTU-team.R before R/LTU-league-comparison.R.")
}
url_men <- "https://embed-api.eui.connect.sportradar.com/v1/embed/303/fixtures?state=eJyrVipWslJQMk4zMkk1MjTUNbM0sNA1NEwz0E1KMTbVNTBPSUs2TDE1s0hNUtJRUMoBKU7N03X1U6oFALDFDrc"

# 2. Functions for results and team networks ----

# Each edge points from the losing team to the winner, weighted by winning margin.
build_team_network <- function(all_results) {
  # directed edge: loser -> winner, weight = margin of victory
  # (a bigger blowout = a stronger signal of the winner's strength)
  edges <- all_results %>%
    mutate(
      loser = if_else(winner == team1, team2, team1)
    ) %>%
    transmute(from = loser, to = winner, weight = margin)

  team_names <- unique(c(all_results$team1, all_results$team2))

  g <- graph_from_data_frame(edges, directed = TRUE, vertices = team_names)
  E(g)$weight <- edges$weight

  g
}

# Keep the existing column name for compatibility with the report and slides.
# The values are team eigenvector centralities, rather than network centralisation.
compute_team_eigenvector <- function(all_results, season_label = "2025") {
  g <- build_team_network(all_results)

  ec <- eigen_centrality(g, directed = TRUE, weights = E(g)$weight)$vector

  tibble(
    season = season_label,
    team = names(ec),
    eigenvector_centralization = as.numeric(ec)
  ) %>%
    arrange(desc(eigenvector_centralization))
}

pull_season_results <- function(fixtures_url, league_label) {
  res <- GET(fixtures_url, add_headers("User-Agent" = "Mozilla/5.0"))
  if (status_code(res) != 200) stop("Fixtures fetch failed: ", status_code(res))

  fixtures_data <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)

  fixtures_df_out <- fixtures_data$data$fixtures %>%
    mutate(
      state = str_extract(link, "(?<=~w=f~).+"),
      team1 = map_chr(competitors, ~ .x$name[1]),
      team2 = map_chr(competitors, ~ .x$name[2])
    )

  fixtures_df_out %>%
    mutate(
      score1 = map_dbl(competitors, ~ as.numeric(.x$score[1])),
      score2 = map_dbl(competitors, ~ as.numeric(.x$score[2])),
      status1 = map_chr(competitors, ~ .x$resultStatus[1])
    ) %>%
    filter(!is.na(score1), !is.na(score2), status1 == "CONFIRMED") %>%
    select(fixtureId, team1, team2, score1, score2, round, startTimeLocal) %>%
    mutate(
      winner = if_else(score1 > score2, team1, team2),
      margin = abs(score1 - score2),
      league = league_label
    )
}

compute_league_eigenvector <- function(results, league_label) {
  g <- build_team_network(results)
  ec <- eigen_centrality(g, directed = TRUE, weights = E(g)$weight)$vector
  tibble(league = league_label, team = names(ec), eigenvector_centralization = as.numeric(ec)) %>%
    arrange(desc(eigenvector_centralization))
}

plot_league_eigenvector <- function(scores, title) {
  ggplot(scores, aes(
    x = reorder(team, eigenvector_centralization),
    y = eigenvector_centralization, fill = eigenvector_centralization
  )) +
    geom_col() + coord_flip() +
    scale_fill_distiller(palette = "RdBu", direction = 1) +
    theme_classic(base_size = 14) +
    labs(title = title, x = "Team", y = "Eigenvector centrality", fill = NULL)
}

# 3. Summarise the women's competition ----

all_results <- fixtures_df %>%
  mutate(
    score1 = map_dbl(competitors, ~ as.numeric(.x$score[1])),
    score2 = map_dbl(competitors, ~ as.numeric(.x$score[2])),
    status1 = map_chr(competitors, ~ .x$resultStatus[1])
  ) %>%
  filter(!is.na(score1), !is.na(score2), status1 == "CONFIRMED") %>%
  select(fixtureId, team1, team2, score1, score2, round, startTimeLocal) %>%
  mutate(
    winner = if_else(score1 > score2, team1, team2),
    margin = abs(score1 - score2)
  )

team_eigenvector_2025 <- compute_team_eigenvector(all_results)
ubl_balance_2025 <- tibble(
  season = "2025",
  eigenvector_gini = gini_coefficient(team_eigenvector_2025$eigenvector_centralization)
)
women_participation <- bind_rows(
  all_results %>% select(team = team1),
  all_results %>% select(team = team2)
) %>% count(team, sort = TRUE)
print(women_participation)
print(team_eigenvector_2025)

# 4. Summarise the men's competition ----

all_results_men <- pull_season_results(url_men, "UBL Men 2025")
team_eigenvector_men <- compute_league_eigenvector(all_results_men, "UBL Men 2025")
gini_men <- gini_coefficient(team_eigenvector_men$eigenvector_centralization)
men_participation <- bind_rows(
  all_results_men %>% select(team = team1),
  all_results_men %>% select(team = team2)
) %>% count(team, sort = TRUE)
print(men_participation)
print(team_eigenvector_men)

# 5. Compare the competitions ----

league_balance_comparison <- bind_rows(
  tibble(league = "UBL Women 2025", gini = ubl_balance_2025$eigenvector_gini,
         top_team = team_eigenvector_2025$team[1], top_score = team_eigenvector_2025$eigenvector_centralization[1]),
  tibble(league = "UBL Men 2025", gini = gini_men,
         top_team = team_eigenvector_men$team[1], top_score = team_eigenvector_men$eigenvector_centralization[1])
)
print(league_balance_comparison)

# A higher Gini means these network scores are more concentrated.
# It does not, by itself, explain why a competition is more or less balanced.

# 6. Draw and save the league figures ----

women_eigenvector_plot <- plot_league_eigenvector(
  team_eigenvector_2025, "UBL women's team eigenvector centrality (2025)"
)
men_eigenvector_plot <- plot_league_eigenvector(
  team_eigenvector_men, "UBL men's team eigenvector centrality (2025)"
)
print(women_eigenvector_plot)
print(men_eigenvector_plot)
ggsave(
  filename = file.path(ltu_paths$figures, "LTU_team_eigenvector_women.png"),
  plot = women_eigenvector_plot, width = 10, height = 7, dpi = 300, bg = "white"
)
ggsave(
  filename = file.path(ltu_paths$figures, "LTU_team_eigenvector_men.png"),
  plot = men_eigenvector_plot, width = 10, height = 7, dpi = 300, bg = "white"
)

# 7. Save the comparison tables ----

write_ltu_table(team_eigenvector_2025, "LTU_team_eigenvector_women.csv")
write_ltu_table(team_eigenvector_men, "LTU_team_eigenvector_men.csv")
write_ltu_table(league_balance_comparison, "LTU_league_balance_comparison.csv")
