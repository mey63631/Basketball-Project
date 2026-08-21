# Data Documentation

## data/raw/
Not committed (gitignored). Contains raw play-by-play data scraped
from UniSport's Sportradar embed API (embed-api.eui.connect.sportradar.com),
specifically LTU vs UTAS fixtures.

Files:
- fixtures_season.rds — full season fixtures list
- pbp_LTU_UTAS.rds — cleaned play-by-play events for LTU vs UTAS matches

To regenerate: run `analysis/01-scrape-data.R`, which sources
`R/scrape_pbp.R`.

Note: fixture `state` tokens in the fixtures URL are signed and may
expire over time — if the initial fetch fails, a fresh fixtures URL
may be needed from the UniSport site.