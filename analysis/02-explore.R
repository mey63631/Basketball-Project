library(dplyr)
library(ggplot2)

pbp <- readRDS("data/raw/pbp_LTU_UTAS.rds")

# quick sanity checks
glimpse(pbp)
count(pbp, action, sort = TRUE)
