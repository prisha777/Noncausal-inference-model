# Prepare a blinded, stratified human-coding sample for mandate stance and frames.
# This script creates an annotation instrument; it does not create human labels.
source("R/00_config.R")

required <- c("data.table", "stringr")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing packages: ", paste(missing, collapse = ", "))

tweets_path <- file.path(dir_intermediate, "tweets_normalized.csv")
labels_path <- file.path(dir_intermediate, "tweet_labels_keyword_baseline.csv")
tweets <- data.table::fread(
  tweets_path,
  select = c("tweet_id", "created_at", "screen_name", "state", "party", "chamber", "month", "text_norm")
)
labels <- data.table::fread(labels_path, select = "tweet_id")
candidate <- merge(tweets, labels, by = "tweet_id", all = FALSE)
candidate <- candidate[party %chin% c("Democratic", "Republican")]
candidate[, month := data.table::as.IDate(month)]
candidate[, phase := data.table::fcase(
  month <= data.table::as.IDate("2020-05-01"), "Emergency onset",
  month <= data.table::as.IDate("2020-12-01"), "Pre-vaccine contestation",
  month <= data.table::as.IDate("2021-06-01"), "Vaccine rollout",
  month <= data.table::as.IDate("2021-12-01"), "Delta and mandates",
  default = "Omicron period"
)]

mandate_pattern <- paste0(
  "\\b(mask mandate|vaccine mandate|forced vaccination|medical freedom|",
  "do not comply|don't comply|lockdown|shutdown)\\b"
)
candidate[, dictionary_screen := as.integer(
  stringr::str_detect(stringr::str_to_lower(text_norm), mandate_pattern)
)]
candidate[, stratum := paste(party, phase, dictionary_screen, sep = "__")]
candidate[, stratum_population := .N, by = stratum]

set.seed(20260810)
target_per_stratum <- 75L
sample <- candidate[, .SD[sample.int(.N, min(.N, target_per_stratum))], by = stratum]
sample[, stratum_sample := .N, by = stratum]
sample[, sample_weight := stratum_population / stratum_sample]
sample <- sample[sample(.N)]
sample[, annotation_id := sprintf("MS%04d", seq_len(.N))]

coding_sheet <- sample[, .(
  annotation_id,
  tweet_date = created_at,
  phase,
  tweet_text = text_norm,
  stance_label = "",
  frame_freedom_choice = "",
  frame_government_overreach = "",
  frame_safety_effectiveness = "",
  frame_economic_harm = "",
  frame_institutional_distrust = "",
  frame_constitutional_rights = "",
  coder_id = "",
  coder_confidence = "",
  coding_notes = ""
)]
coding_key <- sample[, .(
  annotation_id, tweet_id, screen_name, party, chamber, state, phase,
  dictionary_screen, stratum, stratum_population, stratum_sample, sample_weight
)]

validation_dir <- file.path(dir_intermediate, "validation")
dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)
master_path <- file.path(validation_dir, "mandate_stance_frame_coding_sheet.csv")
key_path <- file.path(validation_dir, "mandate_stance_frame_coding_key.csv")
coder_a_path <- file.path(validation_dir, "mandate_stance_frame_coder_A.csv")
coder_b_path <- file.path(validation_dir, "mandate_stance_frame_coder_B.csv")
coder_workbook_dir <- file.path(dir_outputs, "stance-frame-coding-20260815")
coder_a_workbook <- file.path(coder_workbook_dir, "mandate_stance_frame_coder_A.xlsx")
coder_b_workbook <- file.path(coder_workbook_dir, "mandate_stance_frame_coder_B.xlsx")

# Once coding begins, the annotation IDs must remain attached to the same tweets.
# Refuse to replace the key if a rerun would change that mapping.
if (any(file.exists(c(coder_a_path, coder_b_path, coder_a_workbook, coder_b_workbook)))) {
  if (!file.exists(key_path)) {
    stop("Coder files exist but the protected coding key is missing.")
  }
  existing_key <- data.table::fread(key_path, select = c("annotation_id", "tweet_id"))
  old_map <- existing_key[order(annotation_id), .(annotation_id, tweet_id = as.character(tweet_id))]
  new_map <- coding_key[order(annotation_id), .(annotation_id, tweet_id = as.character(tweet_id))]
  if (!identical(old_map, new_map)) {
    stop("The regenerated sample would change annotation IDs. Existing coder files and key were preserved.")
  }
}

data.table::fwrite(coding_sheet, master_path)
data.table::fwrite(coding_key, key_path)

# Create two independently ordered coder files once. Never overwrite them: they may
# contain completed human work when this preparation script is rerun.
write_coder_file_once <- function(sheet, coder_name, seed, path) {
  if (file.exists(path)) {
    message("Preserved existing human-coding file: ", path)
    return(invisible(FALSE))
  }
  coder_sheet <- data.table::copy(sheet)
  coder_sheet[, coder_id := coder_name]
  set.seed(seed)
  coder_sheet <- coder_sheet[sample.int(.N)]
  data.table::fwrite(coder_sheet, path)
  message("Created ", coder_name, " coding file: ", path)
  invisible(TRUE)
}

write_coder_file_once(
  coding_sheet, "A", 20260811L,
  coder_a_path
)
write_coder_file_once(
  coding_sheet, "B", 20260812L,
  coder_b_path
)

summary <- sample[, .(
  population = unique(stratum_population),
  sample_n = .N,
  sampling_weight = unique(sample_weight)
), by = .(party, phase, dictionary_screen, stratum)]
data.table::fwrite(summary, file.path(dir_outputs, "mandate_stance_annotation_sample_summary.csv"))
message("Prepared ", nrow(coding_sheet), " blinded annotation rows across ", nrow(summary), " strata.")
