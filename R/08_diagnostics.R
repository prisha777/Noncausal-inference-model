# Descriptive diagnostics for the communication panel.
source("R/00_config.R")
panel_path <- file.path(dir_processed, "state_month_associational_panel.csv")
if (!file.exists(panel_path)) stop("Missing state_month_associational_panel.csv. Run R/04_build_communication_measures.R and R/06_make_panel.R first.")
panel <- data.table::fread(panel_path)
coverage <- panel[, .(
  months = .N,
  state_months_with_labeled_messages = sum(labeled_tweets > 0, na.rm = TRUE),
  all_archive_tweets = sum(all_archive_tweets, na.rm = TRUE),
  labeled_tweets = sum(labeled_tweets, na.rm = TRUE),
  severity_sum = sum(severity_sum, na.rm = TRUE)
), by = state][order(-severity_sum)]
data.table::fwrite(coverage, file.path(dir_outputs, "state_communication_coverage.csv"))

# Engagement fields are audited before any engagement association is attempted.
tweets_path <- file.path(dir_intermediate, "tweets_normalized.csv")
if (file.exists(tweets_path)) {
  tweets <- data.table::fread(tweets_path, select = intersect(c("tweet_id", "retweet_count", "favorite_count"), names(data.table::fread(tweets_path, nrows = 0))))
  engagement_fields <- intersect(c("retweet_count", "favorite_count"), names(tweets))
  engagement_audit <- data.table::rbindlist(lapply(engagement_fields, function(field) {
    data.table::data.table(field = field, total_tweets = nrow(tweets), observed_values = sum(!is.na(tweets[[field]])), coverage = mean(!is.na(tweets[[field]])))
  }), fill = TRUE)
  if (!nrow(engagement_audit)) engagement_audit <- data.table::data.table(field = character(), total_tweets = integer(), observed_values = integer(), coverage = numeric())
  data.table::fwrite(engagement_audit, file.path(dir_outputs, "engagement_data_readiness.csv"))
}
if (requireNamespace("ggplot2", quietly = TRUE)) {
  p <- ggplot2::ggplot(panel, ggplot2::aes(x = as.Date(month), y = mean_tweet_severity, group = state)) +
    ggplot2::geom_line(alpha = 0.25) +
    ggplot2::labs(x = NULL, y = "Mean labeled-tweet severity", title = "Congressional COVID communication severity by state") +
    ggplot2::theme_minimal()
  ggplot2::ggsave(file.path(dir_outputs, "state_communication_severity_trends.png"), p, width = 9, height = 5, dpi = 200)
}
print(coverage[1:min(10, .N)])
