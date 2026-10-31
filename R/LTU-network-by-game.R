# LTU pair connections within each game
# Run after source("R/LTU-team.R"), from the project root.
# These are in-sample descriptions using the season model, not held-out predictions.

# 1. Packages, inputs and display settings ----

library(tidyr)
library(purrr)
library(igraph)
library(ggraph)
library(ggplot2)
library(dplyr)

if (!exists("ltu_connection") || !exists("ltu_minutes") ||
    !exists("ltu_paths") || !exists("write_ltu_table")) {
  stop("Run R/LTU-team.R before R/LTU-network-by-game.R.")
}
min_game_minutes <- 2
# Two minutes keeps the figure readable; it is not a reliability guarantee.
season_model <- attr(ltu_connection, "rapm_model")
if (is.null(season_model)) {
  stop("Recreate ltu_connection using compute_connection_scores().")
}

# 2. Average each pair's residuals within a game ----

game_pair_scores <- season_model$stint_predictions %>%
  mutate(pairs = map(
    lineup, ~ combn(sort(as.character(.x)), 2, simplify = FALSE)
  )) %>%
  select(fixtureId, opponent, duration_sec, stint_residual, pairs) %>%
  unnest_longer(pairs) %>%
  mutate(id_1 = map_chr(pairs, 1), id_2 = map_chr(pairs, 2)) %>%
  group_by(fixtureId, opponent, id_1, id_2) %>%
  summarise(
    minutes_together = sum(duration_sec) / 60,
    n_stints = n(),
    connection_score = weighted.mean(stint_residual, w = duration_sec),
    .groups = "drop"
  )

# 3. Prepare common nodes and game labels ----

# Retain fixture-ID order from the supplied plot. These are display numbers,
# not chronological round numbers; fixture IDs are also saved in the tables.
game_key <- season_model$stint_predictions %>%
  distinct(fixtureId, opponent) %>% arrange(fixtureId) %>%
  mutate(game = paste0("Game ", row_number(), " vs ", opponent))

nodes <- ltu_minutes %>% transmute(
  name = as.character(personId), player_name, total_minutes
)
edges <- game_pair_scores %>%
  filter(minutes_together >= min_game_minutes) %>%
  left_join(game_key, by = c("fixtureId", "opponent")) %>%
  transmute(
    from = id_1, to = id_2,
    game = factor(game, levels = game_key$game),
    minutes_together, connection_score, n_stints
  )
if (nrow(edges) == 0) stop("No pairs meet the within-game minutes threshold.")

g_games <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
# Fixed coordinates and common scales make the panels comparable.
game_layout <- create_layout(g_games, layout = "circle")
colour_limit <- max(abs(edges$connection_score), na.rm = TRUE)
if (colour_limit == 0) colour_limit <- 1

# 4. Draw the faceted network ----

set.seed(123)
game_network_plot <- ggraph(game_layout) +
  geom_edge_link(
    aes(width = minutes_together, colour = connection_score), alpha = 0.75
  ) +
  geom_node_point(aes(size = total_minutes), colour = "grey25") +
  geom_node_text(aes(label = player_name), repel = TRUE, size = 2.5) +
  scale_edge_width(range = c(0.3, 2.5), name = "Shared minutes\nin this game") +
  scale_edge_colour_gradient2(
    low = "firebrick", mid = "grey85", high = "steelblue",
    midpoint = 0, limits = c(-colour_limit, colour_limit),
    name = "Net points/min\nabove prediction"
  ) +
  scale_size_continuous(range = c(2, 6), name = "Season minutes") +
  facet_edges(~ game, ncol = 2, drop = FALSE) +
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0)
  ) +
  labs(
    title = "LTU player-pair connections by game",
    subtitle = paste0("Pairs sharing at least ", min_game_minutes, " minutes within a game"),
    caption = paste(
      "Common season-model baseline; descriptive in-sample scores.",
      "Players and colour scales are consistent across panels.",
      "Nodes appear in every panel, including players who did not play."
    )
  )
print(game_network_plot)

# 5. Save the figure and game-level values ----

ggsave(
  filename = file.path(ltu_paths$figures, "LTU_connection_network_by_game.png"),
  plot = game_network_plot,
  width = 14, height = 4 * ceiling(nrow(game_key) / 2),
  dpi = 300, bg = "white"
)
write_ltu_table(game_pair_scores, "LTU_connection_scores_by_game.csv")
write_ltu_table(game_key, "LTU_game_labels.csv")
