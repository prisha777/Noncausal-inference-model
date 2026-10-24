# Audit whether tweet labels are ready for descriptive and associational reporting.
# This is a measurement-quality gate, not a causal-identification gate.
source("R/00_config.R")

tweets_path <- file.path(dir_intermediate, "tweets_normalized.csv")
labels_path <- file.path(dir_intermediate, "tweet_labels_normalized.csv")
candidate_path <- file.path(dir_outputs, "llm_labeling_batch.csv")
validation_path <- file.path(dir_outputs, "label_validation_readiness.csv")
signoff_path <- file.path(dir_outputs, "label_validation_signoff.csv")
batch_dir <- file.path(dir_intermediate, "openai_label_batches")
audit_path <- file.path(dir_outputs, "label_measurement_readiness.csv")

if (!file.exists(tweets_path) || !file.exists(labels_path)) stop("Missing normalized tweets or labels. Run R/01_ingest_tweets.R and the labeling step first.")
tweets <- data.table::fread(tweets_path, select = "tweet_id")
labels <- data.table::fread(labels_path)[!is.na(tweet_id)]
tweet_ids <- unique(as.character(tweets$tweet_id))
label_ids <- unique(as.character(labels$tweet_id))
sources <- unique(stats::na.omit(as.character(labels$label_source)))
keyword_baseline_only <- length(sources) > 0 && all(sources == "keyword_baseline_not_for_final_inference")

candidate_ids <- if (file.exists(candidate_path)) unique(as.character(data.table::fread(candidate_path, select = "tweet_id")$tweet_id)) else character()
result_files <- if (dir.exists(batch_dir)) list.files(batch_dir, pattern = "(output|result|results).*\\.jsonl$", full.names = TRUE) else character()
valid_label_rows <- labels$label_source == "llm_structured_output" & !is.na(labels$is_covid_related) & labels$category %in% c(
  "covid_misinformation", "vaccine_hesitancy_promotion", "mask_mandate_opposition", "health_institution_delegitimization",
  "accurate_health_information", "policy_opinion_or_normative_claim", "unrelated_or_insufficient_context"
) & is.finite(suppressWarnings(as.numeric(labels$misinformation_score))) & suppressWarnings(as.numeric(labels$misinformation_score)) %in% 0:5 &
  is.finite(suppressWarnings(as.numeric(labels$confidence))) & suppressWarnings(as.numeric(labels$confidence)) >= 0 & suppressWarnings(as.numeric(labels$confidence)) <= 1
valid_label_ids <- unique(as.character(labels$tweet_id[valid_label_rows]))

validation_status <- "not_started"; completed_human_labels <- 0L
if (file.exists(validation_path)) {
  validation <- data.table::fread(validation_path)
  if (nrow(validation)) {
    validation_status <- as.character(validation$status[1])
    if ("binary_labels_completed" %in% names(validation)) completed_human_labels <- as.integer(validation$binary_labels_completed[1])
  }
}
structured_labels_only <- length(sources) > 0 && all(sources == "llm_structured_output")
labels_complete_for_candidates <- length(candidate_ids) > 0 && length(setdiff(candidate_ids, valid_label_ids)) == 0 && !anyDuplicated(labels$tweet_id)
human_validation_complete <- identical(validation_status, "complete")
validation_approved <- FALSE
if (file.exists(signoff_path)) {
  signoff <- data.table::fread(signoff_path)
  if (nrow(signoff) && "approved" %in% names(signoff)) validation_approved <- tolower(as.character(signoff$approved[1])) %in% c("true", "yes", "1")
}
label_measurement_ready <- structured_labels_only && labels_complete_for_candidates && human_validation_complete && validation_approved

audit <- data.table::data.table(
  metric = c("normalized_unique_tweets", "unique_labeled_tweets", "valid_structured_label_rows", "label_coverage_all_tweets", "llm_candidate_tweets", "candidate_tweets_missing_valid_labels", "label_sources", "keyword_baseline_only", "batch_result_files_present", "human_validation_status", "human_binary_labels_completed", "structured_labels_only", "labels_complete_for_candidate_universe", "validation_metrics_reviewed_and_approved", "label_measurement_ready_for_descriptive_reporting"),
  value = c(as.character(length(tweet_ids)), as.character(length(label_ids)), as.character(sum(valid_label_rows)), sprintf("%.4f", length(label_ids) / length(tweet_ids)), as.character(length(candidate_ids)), as.character(length(setdiff(candidate_ids, valid_label_ids))), paste(sources, collapse = ";"), as.character(keyword_baseline_only), as.character(length(result_files)), validation_status, as.character(completed_human_labels), as.character(structured_labels_only), as.character(labels_complete_for_candidates), as.character(validation_approved), as.character(label_measurement_ready)),
  status = c(rep("informational", 11), ifelse(structured_labels_only, "pass", "fail"), ifelse(labels_complete_for_candidates, "pass", "fail"), ifelse(validation_approved, "pass", "fail"), ifelse(label_measurement_ready, "pass", "fail"))
)
data.table::fwrite(audit, audit_path)
assign("label_measurement_ready_for_descriptive_reporting", label_measurement_ready, envir = .GlobalEnv)
assign("label_measurement_readiness_audit_path", audit_path, envir = .GlobalEnv)
message("Wrote label-measurement audit to ", audit_path, ". Reporting readiness: ", label_measurement_ready)
