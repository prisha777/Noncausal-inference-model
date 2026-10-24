# Build descriptive communication measures from scored congressional tweets.
# These are measures of observed elite message supply, not constituent exposure.
source("R/00_config.R")

tweets_path <- file.path(dir_intermediate, "tweets_normalized.csv")
labels_path <- file.path(dir_intermediate, "tweet_labels_normalized.csv")
members_path <- file.path(dir_intermediate, "members_normalized.csv")

if (!file.exists(tweets_path) || !file.exists(labels_path)) {
  stop("Need tweets_normalized.csv and tweet_labels_normalized.csv before building communication measures.")
}

tweets <- data.table::fread(tweets_path)
labels <- data.table::fread(labels_path)
members <- if (file.exists(members_path)) data.table::fread(members_path) else data.table::data.table()

# Archive volume is retained separately from labeled-message volume. Unlabeled
# tweets are never silently assigned a zero misinformation score.
all_archive_month <- data.table::copy(tweets)
all_archive_month[, state := normalize_state(state)]
all_archive_month <- all_archive_month[!is.na(state), .(
  all_archive_tweets = .N,
  archive_members_observed = data.table::uniqueN(screen_name)
), by = .(state, month)]

dt <- merge(tweets, labels, by = "tweet_id", all = FALSE)
if (nrow(members) > 0) {
  # Prefer Bioguide IDs when they are observed; otherwise use a case-insensitive
  # Twitter-handle crosswalk. The Zenodo archive has handles but no Bioguide IDs.
  use_bioguide <- all(c("bioguide_id") %in% names(dt)) && sum(!is.na(dt$bioguide_id) & nzchar(dt$bioguide_id)) > 0 &&
    all(c("bioguide_id") %in% names(members)) && sum(!is.na(members$bioguide_id) & nzchar(members$bioguide_id)) > 0
  if (use_bioguide) {
    dt <- merge(dt, members[, .SD, .SDcols = intersect(names(members), c("bioguide_id", "screen_name", "state", "party", "chamber"))],
      by = "bioguide_id", all.x = TRUE, suffixes = c("", "_member"))
  } else if ("screen_name" %in% names(dt) && "screen_name" %in% names(members)) {
    dt[, member_join_key := tolower(sub("^@", "", trimws(screen_name)))]
    member_crosswalk <- data.table::copy(members)
    member_crosswalk[, member_join_key := tolower(sub("^@", "", trimws(screen_name)))]
    member_crosswalk <- unique(member_crosswalk[!is.na(member_join_key) & nzchar(member_join_key)], by = "member_join_key")
    dt <- merge(dt, member_crosswalk[, .SD, .SDcols = intersect(names(member_crosswalk), c("member_join_key", "bioguide_id", "state", "party", "chamber"))],
      by = "member_join_key", all.x = TRUE, suffixes = c("", "_member"))
    dt[, member_join_key := NULL]
  }
  for (col in c("bioguide_id", "state", "party", "chamber")) {
    member_col <- paste0(col, "_member")
    if (member_col %in% names(dt)) {
      dt[, (col) := data.table::fcoalesce(as.character(get(col)), as.character(get(member_col)))]
    }
  }
}

dt[, misinformation_score := as.numeric(misinformation_score)]
dt[, is_high_severity := misinformation_score >= 3]
dt[, state := normalize_state(state)]
dt[, bioguide_id := as.character(bioguide_id)]
dt[, screen_name := as.character(screen_name)]

member_month <- dt[!is.na(state), .(
  labeled_tweets = .N,
  covid_related_tweets = sum(is_covid_related %in% TRUE, na.rm = TRUE),
  high_severity_tweets = sum(is_high_severity, na.rm = TRUE),
  severity_sum = sum(misinformation_score, na.rm = TRUE),
  mean_tweet_severity = mean(misinformation_score, na.rm = TRUE),
  max_tweet_severity = max(misinformation_score, na.rm = TRUE),
  observed_retweets = sum(!is.na(retweet_count)),
  observed_favorites = sum(!is.na(favorite_count))
), by = .(month, bioguide_id, screen_name, member_name, state, party, chamber)]
member_month[, high_severity_share := data.table::fifelse(labeled_tweets > 0, high_severity_tweets / labeled_tweets, NA_real_)]
member_month[, communication_intensity_z := as.numeric(scale(mean_tweet_severity))]

state_month <- member_month[, .(
  members_observed = data.table::uniqueN(data.table::fcoalesce(bioguide_id, screen_name)),
  labeled_tweets = sum(labeled_tweets, na.rm = TRUE),
  covid_related_tweets = sum(covid_related_tweets, na.rm = TRUE),
  high_severity_tweets = sum(high_severity_tweets, na.rm = TRUE),
  severity_sum = sum(severity_sum, na.rm = TRUE),
  mean_member_severity = mean(mean_tweet_severity, na.rm = TRUE),
  mean_tweet_severity = weighted.mean(mean_tweet_severity, w = labeled_tweets, na.rm = TRUE),
  high_severity_share = weighted.mean(high_severity_share, w = labeled_tweets, na.rm = TRUE),
  republican_member_share = mean(stringr::str_detect(stringr::str_to_lower(party), "rep|gop"), na.rm = TRUE),
  members_with_retweet_counts = sum(observed_retweets > 0),
  members_with_favorite_counts = sum(observed_favorites > 0)
), by = .(state, month)]
state_month[, communication_intensity_z := as.numeric(scale(mean_tweet_severity))]
state_month <- merge(state_month, all_archive_month, by = c("state", "month"), all = TRUE)

# Retain only outputs named for what they measure.
data.table::fwrite(member_month, file.path(dir_processed, "member_month_communication.csv"))
data.table::fwrite(state_month, file.path(dir_processed, "state_month_communication.csv"))
message("Wrote member- and state-month communication measures. These represent labeled elite messages, not audience exposure.")
