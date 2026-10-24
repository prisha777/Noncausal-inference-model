source("R/00_config.R")

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x
batch_dir <- file.path(dir_intermediate, "openai_label_batches")
result_files <- if (dir.exists(batch_dir)) list.files(batch_dir, pattern = "(output|result|results).*\\.jsonl$", full.names = TRUE) else character()
candidate_path <- file.path(dir_outputs, "llm_labeling_batch.csv")
valid_categories <- c(
  "covid_misinformation", "vaccine_hesitancy_promotion", "mask_mandate_opposition",
  "health_institution_delegitimization", "accurate_health_information",
  "policy_opinion_or_normative_claim", "unrelated_or_insufficient_context"
)

parse_one_line <- function(line) {
  obj <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(obj)) return(data.table::data.table(tweet_id = NA_character_, label_source = "llm_structured_output", parse_error = "Invalid batch JSONL record"))
  custom_id <- obj$custom_id %||% NA_character_
  tweet_id <- sub("^tweet_", "", custom_id)
  error_message <- obj$error$message %||% obj$response$body$error$message %||% NA_character_
  content <- obj$response$body$choices[[1]]$message$content %||% NA_character_
  parsed <- tryCatch(jsonlite::fromJSON(content), error = function(e) NULL)
  if (is.null(parsed)) {
    return(data.table::data.table(tweet_id = tweet_id, label_source = "llm_structured_output",
      is_covid_related = NA, category = NA_character_, misinformation_score = NA_real_, confidence = NA_real_,
      rationale = NA_character_, parse_error = ifelse(is.na(error_message), "Could not parse JSON content", error_message)))
  }
  data.table::data.table(
    tweet_id = tweet_id, label_source = "llm_structured_output", is_covid_related = as.logical(parsed$is_covid_related),
    category = as.character(parsed$category), misinformation_score = as.numeric(parsed$misinformation_score),
    confidence = as.numeric(parsed$confidence), rationale = as.character(parsed$rationale), parse_error = error_message
  )
}

if (!length(result_files)) {
  stop("No batch output JSONL files found in ", batch_dir, ". Save downloaded files with names containing output/result/results and rerun.")
}
labels <- data.table::rbindlist(lapply(result_files, function(path) {
  message("Parsing ", basename(path))
  data.table::rbindlist(lapply(readLines(path, warn = FALSE, encoding = "UTF-8"), parse_one_line), fill = TRUE)
}), fill = TRUE)
labels[, valid_record := !is.na(tweet_id) & nzchar(tweet_id) & is.na(parse_error) &
  !is.na(is_covid_related) & category %in% valid_categories & is.finite(misinformation_score) &
  misinformation_score %in% 0:5 & is.finite(confidence) & confidence >= 0 & confidence <= 1]
data.table::fwrite(labels, file.path(dir_outputs, "llm_label_parse_audit.csv"))

expected_ids <- if (file.exists(candidate_path)) unique(as.character(data.table::fread(candidate_path, select = "tweet_id")$tweet_id)) else character()
returned_ids <- unique(as.character(labels$tweet_id))
missing_ids <- setdiff(expected_ids, returned_ids)
unexpected_ids <- setdiff(returned_ids, expected_ids)
invalid_n <- sum(!labels$valid_record)
duplicate_n <- sum(duplicated(labels$tweet_id) & !is.na(labels$tweet_id))
if (invalid_n || duplicate_n || length(missing_ids) || length(unexpected_ids)) {
  stop(
    "Batch results failed validation; current labels were not changed. Invalid records: ", invalid_n,
    "; duplicate tweet IDs: ", duplicate_n, "; expected IDs missing: ", length(missing_ids),
    "; unexpected IDs: ", length(unexpected_ids), ". See outputs/llm_label_parse_audit.csv."
  )
}

current_labels <- file.path(dir_intermediate, "tweet_labels_normalized.csv")
if (file.exists(current_labels)) {
  backup_path <- file.path(dir_intermediate, paste0("tweet_labels_normalized_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
  file.copy(current_labels, backup_path)
  message("Backed up previous labels to ", backup_path)
}
final_labels <- labels[, .(tweet_id, label_source, is_covid_related, category, misinformation_score, confidence)]
data.table::fwrite(final_labels, current_labels)
data.table::fwrite(final_labels[, .N, by = .(category, misinformation_score)][order(-misinformation_score, -N)], file.path(dir_outputs, "llm_label_distribution.csv"))
message("Validated and wrote ", nrow(final_labels), " structured labels to ", current_labels)
