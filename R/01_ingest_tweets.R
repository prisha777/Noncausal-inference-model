source("R/00_config.R")

# Default to the complete study-period archive. Add other sources deliberately:
# TWEET_SOURCES=zenodo_miller_jiang,alexlitel Rscript R/01_ingest_tweets.R
study_start <- as.Date(Sys.getenv("ANALYSIS_START_DATE", unset = "2020-01-01"))
study_end <- as.Date(Sys.getenv("ANALYSIS_END_DATE", unset = "2022-06-30"))
if (is.na(study_start) || is.na(study_end) || study_end < study_start) {
  stop("ANALYSIS_START_DATE and ANALYSIS_END_DATE must be valid YYYY-MM-DD dates with end >= start.")
}

available_sources <- c(
  alexlitel = "alexlitel",
  zenodo_miller_jiang = "zenodo_miller_jiang",
  kaggle_congress_tweets = "kaggle_congress_tweets"
)
requested_sources <- trimws(strsplit(Sys.getenv("TWEET_SOURCES", unset = "zenodo_miller_jiang"), ",", fixed = TRUE)[[1]])
requested_sources <- requested_sources[nzchar(requested_sources)]
unknown_sources <- setdiff(requested_sources, names(available_sources))
if (length(unknown_sources)) stop("Unknown TWEET_SOURCES value(s): ", paste(unknown_sources, collapse = ", "))

normalize_tweet_table <- function(dt, source_name) {
  data.table::setDT(dt)
  out <- data.table::data.table(
    source = source_name,
    tweet_id = as.character(coalesce_columns(dt, c("tweet_id", "id", "id_str", "status_id", "TweetId"))),
    created_at = as.character(coalesce_columns(dt, c("created_at", "created", "date", "datetime", "timestamp", "time"))),
    screen_name = as.character(coalesce_columns(dt, c("screen_name", "user_screen_name", "username", "handle", "UserName", "account"))),
    member_name = as.character(coalesce_columns(dt, c("member_name", "name", "full_name", "Name", "author_name"))),
    bioguide_id = as.character(coalesce_columns(dt, c("bioguide_id", "bioguide", "BioGuideID", "member_id"))),
    state = normalize_state(coalesce_columns(dt, c("state", "State", "member_state"))),
    party = as.character(coalesce_columns(dt, c("party", "Party", "member_party"))),
    chamber = as.character(coalesce_columns(dt, c("chamber", "Chamber", "body"))),
    text = as.character(coalesce_columns(dt, c("text", "full_text", "tweet", "content", "Text"))),
    retweet_count = suppressWarnings(as.numeric(coalesce_columns(dt, c("retweet_count", "retweets", "RetweetCount"), default = NA))),
    favorite_count = suppressWarnings(as.numeric(coalesce_columns(dt, c("favorite_count", "favorites", "likes", "FavoriteCount"), default = NA)))
  )
  out[, month := safe_month(created_at)]
  out[, text_norm := stringr::str_squish(text)]
  out <- out[!is.na(tweet_id) & tweet_id != "" & !is.na(text_norm) & text_norm != ""]
  unique(out, by = c("source", "tweet_id", "created_at", "screen_name", "text_norm"))
}

files_in_study_window <- function(files) {
  base_names <- basename(files)
  has_iso_date <- grepl("\\d{4}-\\d{2}-\\d{2}", base_names)
  date_text <- ifelse(has_iso_date, sub(".*?(\\d{4}-\\d{2}-\\d{2}).*", "\\1", base_names), NA_character_)
  file_dates <- as.Date(date_text)
  # Non-dated archive files are retained and filtered after parsing. Dated
  # daily archives outside the study window are skipped before parsing.
  files[is.na(file_dates) | (file_dates >= study_start & file_dates <= study_end)]
}

read_source_dir <- function(subdir, source_name) {
  folder <- file.path(dir_raw, subdir)
  if (!dir.exists(folder)) {
    message("Skipping missing folder: ", folder)
    return(data.table::data.table())
  }
  files <- list.files(folder, pattern = "\\.(csv|tsv|txt|json|jsonl|ndjson|xlsx|xls)$", recursive = TRUE, full.names = TRUE)
  files <- files_in_study_window(files)
  if (!length(files)) {
    message("No study-window files found in: ", folder)
    return(data.table::data.table())
  }
  parts <- lapply(files, function(path) {
    message("Reading ", basename(path), " from ", source_name)
    dt <- tryCatch(read_any_table(path), error = function(e) {
      message("Could not read ", path, ": ", conditionMessage(e))
      data.table::data.table()
    })
    if (!nrow(dt)) return(dt)
    normalize_tweet_table(dt, source_name)
  })
  data.table::rbindlist(parts, fill = TRUE)
}

normalized_path <- file.path(dir_intermediate, "tweets_normalized.csv")
force_reingest <- identical(Sys.getenv("FORCE_REINGEST"), "1")
can_use_cache <- FALSE
if (file.exists(normalized_path) && !force_reingest) {
  cached <- data.table::fread(normalized_path, select = c("source", "month"), showProgress = FALSE)
  cached_sources <- sort(unique(as.character(cached$source)))
  cached_months <- suppressWarnings(as.Date(cached$month))
  raw_files <- unlist(lapply(unname(available_sources[requested_sources]), function(subdir) {
    folder <- file.path(dir_raw, subdir)
    if (!dir.exists(folder)) return(character())
    list.files(folder, pattern = "\\.(csv|tsv|txt|json|jsonl|ndjson|xlsx|xls)$", recursive = TRUE, full.names = TRUE)
  }))
  raw_is_not_newer <- !length(raw_files) || max(file.info(raw_files)$mtime, na.rm = TRUE) <= file.info(normalized_path)$mtime
  can_use_cache <- identical(cached_sources, sort(requested_sources)) && length(cached_months) > 0 &&
    min(cached_months, na.rm = TRUE) >= study_start && max(cached_months, na.rm = TRUE) <= study_end && raw_is_not_newer
}

if (can_use_cache) {
  cached <- data.table::fread(normalized_path, select = c("source", "tweet_id", "month", "screen_name"), showProgress = FALSE)
  coverage <- cached[, .(loaded_rows = .N, loaded_unique_tweet_ids = data.table::uniqueN(tweet_id), retained_rows = .N,
    retained_unique_tweet_ids = data.table::uniqueN(tweet_id), study_start = as.character(study_start),
    study_end = as.character(study_end), source_selected = TRUE), by = source]
  data.table::fwrite(coverage, file.path(dir_outputs, "tweet_source_coverage.csv"))
  message("Reusing verified normalized tweet cache. Set FORCE_REINGEST=1 to rebuild from raw files.")
  print(coverage)
} else {
  tweets <- data.table::rbindlist(
    Map(read_source_dir, unname(available_sources[requested_sources]), requested_sources),
    fill = TRUE
  )

  if (!nrow(tweets)) {
    message("No tweet files ingested. Add raw files under data/raw/<source>/ and rerun.")
  } else {
    tweets <- tweets[!is.na(month) & month >= study_start & month <= study_end]
    loaded_coverage <- tweets[, .(loaded_rows = .N, loaded_unique_tweet_ids = data.table::uniqueN(tweet_id)), by = source]
    # Archives can overlap. Keep one record per tweet ID, preferring the source
    # order supplied in TWEET_SOURCES, so aggregation never counts a tweet twice.
    tweets[, source_priority := match(source, requested_sources)]
    data.table::setorder(tweets, source_priority, month, screen_name)
    tweets <- unique(tweets, by = "tweet_id")
    tweets[, source_priority := NULL]
    data.table::setorder(tweets, month, source, screen_name)
    retained_coverage <- tweets[, .(retained_rows = .N, retained_unique_tweet_ids = data.table::uniqueN(tweet_id)), by = source]
    coverage <- merge(loaded_coverage, retained_coverage, by = "source", all = TRUE)
    coverage[, `:=`(study_start = as.character(study_start), study_end = as.character(study_end), source_selected = TRUE)]
    data.table::fwrite(tweets, normalized_path)
    data.table::fwrite(coverage, file.path(dir_outputs, "tweet_source_coverage.csv"))
    message("Wrote ", nrow(tweets), " unique study-window tweets from: ", paste(unique(tweets$source), collapse = ", "))
    print(coverage)
  }
}
