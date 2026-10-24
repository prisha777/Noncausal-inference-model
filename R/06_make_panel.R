# Create a state-month panel for descriptive and associational analyses.
source("R/00_config.R")

communication_path <- file.path(dir_processed, "state_month_communication.csv")
outcomes_path <- file.path(dir_processed, "state_month_outcomes.csv")

if (!file.exists(communication_path)) {
  stop("Missing state_month_communication.csv. Run R/04_build_communication_measures.R first.")
}

communication <- data.table::fread(communication_path)
outcomes <- if (file.exists(outcomes_path)) data.table::fread(outcomes_path) else data.table::data.table()

states <- sort(unique(communication$state[!is.na(communication$state)]))
months <- seq(min(data.table::as.IDate(communication$month)), max(data.table::as.IDate(communication$month)), by = "month")
panel <- data.table::CJ(state = states, month = data.table::as.IDate(months))
panel <- merge(panel, communication, by = c("state", "month"), all.x = TRUE)

# Zero is appropriate for observed message counts in a state-month with no
# labeled messages; it is not a substitute label for individual unlabeled tweets.
count_cols <- c("members_observed", "labeled_tweets", "covid_related_tweets", "high_severity_tweets", "severity_sum", "all_archive_tweets", "archive_members_observed")
for (col in intersect(count_cols, names(panel))) panel[is.na(get(col)), (col) := 0]
if (nrow(outcomes) > 0) panel <- merge(panel, outcomes, by = c("state", "month"), all.x = TRUE)

data.table::fwrite(panel, file.path(dir_processed, "state_month_associational_panel.csv"))
message("Wrote state_month_associational_panel.csv with ", nrow(panel), " rows.")
