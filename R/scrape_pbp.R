#' Parse ISO 8601 clock string into seconds remaining
#' @param x character vector like "PT9M45S"
#' @return numeric seconds remaining
parse_clock <- function(x) {
  mins <- as.numeric(sub("PT(\\d+)M.*", "\\1", x))
  secs <- as.numeric(sub(".*M(\\d+(?:\\.\\d+)?)S", "\\1", x))
  mins * 60 + secs
}

#' Fetch cleaned play-by-play data for one fixture
#' @param fixture_state character, the state token identifying the fixture
#' @param base character, API base URL
#' @return a tibble of play-by-play events, or NULL if unavailable
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
