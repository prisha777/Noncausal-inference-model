source("R/00_config.R")

labels_dir <- file.path(dir_raw, "labels")
label_files <- if (dir.exists(labels_dir)) list.files(labels_dir, pattern = "\\.(csv|tsv|txt|json|jsonl|ndjson)$", recursive = TRUE, full.names = TRUE) else character()
tweets_path <- file.path(dir_intermediate, "tweets_normalized.csv")

if (length(label_files) > 0) {
  labels <- data.table::rbindlist(lapply(label_files, function(path) {
    dt <- read_any_table(path)
    data.table::data.table(
      tweet_id = as.character(coalesce_columns(dt, c("tweet_id", "id", "id_str", "status_id"))),
      label_source = basename(path),
      is_covid_related = as.logical(coalesce_columns(dt, c("is_covid_related", "covid_related"), default = NA)),
      category = as.character(coalesce_columns(dt, c("category", "label", "misinfo_category"))),
      misinformation_score = suppressWarnings(as.numeric(coalesce_columns(dt, c("misinformation_score", "severity_score", "score"), default = NA))),
      confidence = suppressWarnings(as.numeric(coalesce_columns(dt, c("confidence", "probability"), default = NA)))
    )
  }), fill = TRUE)
  labels <- unique(labels[!is.na(tweet_id) & nzchar(tweet_id)], by = "tweet_id")
  data.table::fwrite(labels, file.path(dir_intermediate, "tweet_labels_normalized.csv"))
  print(labels[, .N, by = .(category)])
} else if (file.exists(tweets_path)) {
  tweets <- data.table::fread(tweets_path, select = c("tweet_id", "text_norm"))
  # Word boundaries avoid accidental substring matches. This is a high-recall
  # candidate screen, not a final misinformation classifier.
  covid_terms <- paste0(
    "(?i)\\b(",
    "covid(?:[- ]?19)?|coronavirus|sars[- ]?cov[- ]?2|pandemic|",
    "vaccin(?:e|es|ated|ation|ating)|booster(?:s)?|mask(?:s|ing)?|",
    "fauci|cdc|nih|world health organization|@who|public health|mandate(?:s)?|lockdown(?:s)?|shutdown(?:s)?|",
    "operation warp speed|hydroxychloroquine|ivermectin",
    ")\\b"
  )
  candidate <- tweets[stringr::str_detect(text_norm, covid_terms)]
  candidate <- unique(candidate, by = "tweet_id")
  prompt_batch <- candidate[, .(
    tweet_id, text = text_norm,
    prompt = paste0(
      "Classify this congressional tweet. Return JSON with keys: ",
      "is_covid_related, category, misinformation_score_0_to_5, confidence_0_to_1. ",
      "Categories: covid_misinformation, vaccine_hesitancy_promotion, ",
      "mask_mandate_opposition, health_institution_delegitimization, ",
      "accurate_health_information, policy_opinion_or_normative_claim, unrelated_or_insufficient_context. Tweet: ",
      text_norm
    )
  )]
  data.table::fwrite(prompt_batch, file.path(dir_outputs, "llm_labeling_batch.csv"))
  message("No structured labels found. Wrote ", nrow(prompt_batch), " candidate tweets to outputs/llm_labeling_batch.csv.")
} else {
  message("No labels or normalized tweets found yet.")
}
