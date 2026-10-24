# Compatibility entry point for the maintained pipeline.
# The former script mixed installation, exploratory analysis, and hard-coded
# paths; it is intentionally retired so sourcing it is safe and reproducible.
source("R/00_config.R")
message(
  "R/preprocess.R is now a compatibility entry point. Run the explicit workflow in README.md, ",
  "beginning with R/01_ingest_tweets.R. No data or labels were changed."
)
invisible(list(
  next_steps = c("R/01_ingest_tweets.R", "R/02_prepare_member_metadata.R", "R/03_prepare_labels.R"),
  study_window = c(start = "2020-01-01", end = "2022-06-30")
))
