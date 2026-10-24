# Validate two independent stance-and-frame coding files and prepare adjudication.
# This script never invents labels and never treats dictionary matches as human truth.
source("R/00_config.R")

required <- c("data.table")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing packages: ", paste(missing, collapse = ", "))

validation_dir <- file.path(dir_intermediate, "validation")
coder_workbook_dir <- file.path(dir_outputs, "stance-frame-coding-20260815")
choose_coder_path <- function(coder) {
  completed_path <- file.path(root_dir, paste0("mandate_stance_frame_coder_", coder, ".xlsx"))
  workbook_path <- file.path(coder_workbook_dir, paste0("mandate_stance_frame_coder_", coder, ".xlsx"))
  csv_path <- file.path(validation_dir, paste0("mandate_stance_frame_coder_", coder, ".csv"))
  if (file.exists(completed_path)) completed_path else if (file.exists(workbook_path)) workbook_path else csv_path
}
coder_paths <- c(
  A = choose_coder_path("A"),
  B = choose_coder_path("B")
)
key_path <- file.path(validation_dir, "mandate_stance_frame_coding_key.csv")
adjudication_path <- file.path(validation_dir, "mandate_stance_frame_adjudication.csv")
validated_path <- file.path(validation_dir, "mandate_stance_frame_validated_labels.csv")
checksum_path <- file.path(dir_outputs, "stance_frame_coder_file_checksums.csv")
readiness_path <- file.path(dir_outputs, "stance_frame_validation_readiness.csv")
agreement_path <- file.path(dir_outputs, "stance_frame_intercoder_agreement.csv")

stance_levels <- c("support", "neutral", "implicit_opposition", "explicit_opposition")
frame_columns <- c(
  "frame_freedom_choice", "frame_government_overreach", "frame_safety_effectiveness",
  "frame_economic_harm", "frame_institutional_distrust", "frame_constitutional_rights"
)
label_columns <- c("stance_label", frame_columns)
required_columns <- c(
  "annotation_id", "tweet_date", "tweet_text", label_columns,
  "coder_id", "coder_confidence", "coding_notes"
)

normalize_stance <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[- ]+", "_", x)
  x[x %in% c("", "na", "n/a")] <- NA_character_
  x
}

normalize_frame <- function(x) {
  x <- tolower(trimws(as.character(x)))
  out <- rep(NA_integer_, length(x))
  out[x %in% c("1", "yes", "y", "true", "present")] <- 1L
  out[x %in% c("0", "no", "n", "false", "absent")] <- 0L
  out
}

nominal_alpha_two_coders <- function(a, b) {
  keep <- !is.na(a) & !is.na(b)
  a <- as.character(a[keep])
  b <- as.character(b[keep])
  if (!length(a)) return(NA_real_)
  categories <- sort(unique(c(a, b)))
  coincidence <- matrix(0, nrow = length(categories), ncol = length(categories),
                        dimnames = list(categories, categories))
  for (i in seq_along(a)) {
    coincidence[a[i], b[i]] <- coincidence[a[i], b[i]] + 1
    coincidence[b[i], a[i]] <- coincidence[b[i], a[i]] + 1
  }
  n_values <- sum(coincidence)
  observed_disagreement <- sum(coincidence[row(coincidence) != col(coincidence)]) / n_values
  marginals <- rowSums(coincidence)
  expected_disagreement <- (n_values^2 - sum(marginals^2)) / (n_values * (n_values - 1))
  if (!is.finite(expected_disagreement) || expected_disagreement == 0) return(NA_real_)
  1 - observed_disagreement / expected_disagreement
}

cohen_kappa_two_coders <- function(a, b) {
  keep <- !is.na(a) & !is.na(b)
  a <- as.character(a[keep])
  b <- as.character(b[keep])
  if (!length(a)) return(NA_real_)
  categories <- sort(unique(c(a, b)))
  observed_agreement <- mean(a == b)
  marginal_a <- table(factor(a, levels = categories)) / length(a)
  marginal_b <- table(factor(b, levels = categories)) / length(b)
  expected_agreement <- sum(marginal_a * marginal_b)
  if (!is.finite(expected_agreement) || expected_agreement == 1) return(NA_real_)
  (observed_agreement - expected_agreement) / (1 - expected_agreement)
}

agreement_interpretation <- function(alpha) {
  if (!is.finite(alpha)) return("not estimable because the coders used only one category")
  if (alpha >= 0.80) return("reportable reliability")
  if (alpha >= 0.667) return("tentative reliability; document disagreements")
  "revise the code or coding rules before modeling"
}

write_readiness <- function(status, coder_a_completed = 0L, coder_b_completed = 0L,
                            total = NA_integer_, invalid_cells = 0L,
                            disagreements = NA_integer_, detail = "") {
  out <- data.table::data.table(
    status = status,
    total_items = total,
    coder_A_complete_items = coder_a_completed,
    coder_B_complete_items = coder_b_completed,
    invalid_label_cells = invalid_cells,
    items_needing_adjudication = disagreements,
    detail = detail
  )
  data.table::fwrite(out, readiness_path)
  invisible(out)
}

read_coder <- function(path, expected_coder, expected_ids) {
  d <- data.table::as.data.table(read_any_table(path))
  absent <- setdiff(required_columns, names(d))
  if (length(absent)) stop(expected_coder, " file is missing columns: ", paste(absent, collapse = ", "))
  if (anyDuplicated(d$annotation_id)) stop(expected_coder, " file has duplicate annotation_id values.")
  if (!setequal(d$annotation_id, expected_ids)) stop(expected_coder, " file does not contain the expected annotation IDs.")
  d[, stance_label := normalize_stance(stance_label)]
  raw_stance_invalid <- !is.na(d$stance_label) & !d$stance_label %in% stance_levels
  for (column in frame_columns) {
    raw <- d[[column]]
    normalized <- normalize_frame(raw)
    invalid <- !is.na(raw) & nzchar(trimws(as.character(raw))) & is.na(normalized)
    d[, (paste0(column, "__invalid")) := invalid]
    d[, (column) := normalized]
  }
  d[, stance_label__invalid := raw_stance_invalid]
  d[, coder_id := expected_coder]
  d[]
}

run_stance_frame_validation <- function() {
  if (!file.exists(key_path) || any(!file.exists(coder_paths))) {
    missing_files <- c(key_path, coder_paths)[!file.exists(c(key_path, coder_paths))]
    write_readiness("awaiting_coder_files", detail = paste(missing_files, collapse = "; "))
    message("Awaiting coding files. Run R/12_prepare_stance_annotation.R first.")
    return(invisible(FALSE))
  }

  key <- data.table::fread(key_path)
  expected_ids <- key$annotation_id
  a <- read_coder(coder_paths[["A"]], "A", expected_ids)
  b <- read_coder(coder_paths[["B"]], "B", expected_ids)
  data.table::setkey(a, annotation_id)
  data.table::setkey(b, annotation_id)
  a <- a[expected_ids]
  b <- b[expected_ids]

  complete_a <- rowSums(!is.na(a[, ..label_columns])) == length(label_columns)
  complete_b <- rowSums(!is.na(b[, ..label_columns])) == length(label_columns)
  invalid_a <- sum(as.matrix(a[, grep("__invalid$", names(a)), with = FALSE]))
  invalid_b <- sum(as.matrix(b[, grep("__invalid$", names(b)), with = FALSE]))
  invalid_total <- invalid_a + invalid_b

  if (!all(complete_a) || !all(complete_b) || invalid_total > 0) {
    status <- if (invalid_total > 0) "labels_need_correction" else "awaiting_human_labels"
    write_readiness(
      status,
      sum(complete_a), sum(complete_b), nrow(key), invalid_total,
      detail = "Each item needs one allowed stance label and six binary frame labels from both coders."
    )
    message(
      "Human coding is not complete: A ", sum(complete_a), "/", nrow(key),
      "; B ", sum(complete_b), "/", nrow(key), "; invalid cells ", invalid_total, "."
    )
    return(invisible(FALSE))
  }

  metrics <- data.table::rbindlist(lapply(label_columns, function(column) {
    av <- a[[column]]
    bv <- b[[column]]
    alpha <- nominal_alpha_two_coders(av, bv)
    kappa <- cohen_kappa_two_coders(av, bv)
    data.table::data.table(
      measure = column,
      n_double_coded = length(av),
      agreement = mean(av == bv),
      cohen_kappa = kappa,
      krippendorff_alpha_nominal = alpha,
      disagreements = sum(av != bv),
      reliability_gate_0_667 = is.finite(alpha) && alpha >= 0.667,
      interpretation = agreement_interpretation(alpha)
    )
  }))
  data.table::fwrite(metrics, agreement_path)

  stance_confusion <- data.table::as.data.table(table(
    factor(a$stance_label, levels = stance_levels),
    factor(b$stance_label, levels = stance_levels)
  ))
  data.table::setnames(stance_confusion, c("coder_A", "coder_B", "n"))
  data.table::fwrite(stance_confusion, file.path(dir_outputs, "stance_intercoder_confusion.csv"))

  frame_confusion <- data.table::rbindlist(lapply(frame_columns, function(column) {
    z <- data.table::as.data.table(table(
      factor(a[[column]], levels = 0:1),
      factor(b[[column]], levels = 0:1)
    ))
    data.table::setnames(z, c("coder_A", "coder_B", "n"))
    z[, frame := column]
    z
  }))
  data.table::setcolorder(frame_confusion, c("frame", "coder_A", "coder_B", "n"))
  data.table::fwrite(frame_confusion, file.path(dir_outputs, "frame_intercoder_confusion.csv"))

  adjudication <- data.table::data.table(
    annotation_id = expected_ids,
    tweet_date = a$tweet_date,
    tweet_text = a$tweet_text,
    coder_A_stance = a$stance_label,
    coder_B_stance = b$stance_label,
    stance_label = data.table::fifelse(a$stance_label == b$stance_label, a$stance_label, NA_character_)
  )
  for (column in frame_columns) {
    adjudication[, (paste0("coder_A_", column)) := a[[column]]]
    adjudication[, (paste0("coder_B_", column)) := b[[column]]]
    adjudication[, (column) := data.table::fifelse(a[[column]] == b[[column]], a[[column]], NA_integer_)]
  }
  adjudication[, `:=`(
    needs_adjudication = rowSums(is.na(.SD)) > 0,
    adjudication_notes = ""
  ), .SDcols = label_columns]

  disagreement_export <- adjudication[needs_adjudication == TRUE]
  data.table::fwrite(disagreement_export, file.path(dir_outputs, "stance_frame_disagreements.csv"))
  current_checksums <- data.table::data.table(
    coder = names(coder_paths),
    path = unname(coder_paths),
    md5 = unname(tools::md5sum(coder_paths))
  )
  if (!file.exists(adjudication_path)) data.table::fwrite(current_checksums, checksum_path)

  # Agreement is calculated from the original independent labels. Consensus
  # adjudication cannot repair unreliable coding, so stop before adjudication
  # when the stance measure fails the minimum reliability gate.
  stance_gate <- metrics[measure == "stance_label", reliability_gate_0_667][1]
  if (!isTRUE(stance_gate)) {
    stance_alpha <- metrics[measure == "stance_label", krippendorff_alpha_nominal][1]
    write_readiness(
      "stance_reliability_gate_failed", nrow(key), nrow(key), nrow(key), 0L,
      nrow(disagreement_export),
      paste0(
        "Stance reliability was alpha=", sprintf("%.3f", stance_alpha),
        ". Do not adjudicate or model these labels; revise the codebook and have two people independently recode."
      )
    )
    message("Stance reliability failed (alpha=", sprintf("%.3f", stance_alpha), "); models remain blocked.")
    return(invisible(FALSE))
  }

  if (!file.exists(adjudication_path)) {
    data.table::fwrite(adjudication, adjudication_path, na = "")
    write_readiness(
      "awaiting_adjudication", nrow(key), nrow(key), nrow(key), 0L,
      nrow(disagreement_export),
      "Fill only blank final-label cells in mandate_stance_frame_adjudication.csv, then rerun."
    )
    message("Created adjudication file with ", nrow(disagreement_export), " disputed items.")
    return(invisible(FALSE))
  }

  if (!file.exists(checksum_path)) {
    stop("Adjudication exists but the coder-file checksum manifest is missing. Preserve the file and review manually.")
  }
  saved_checksums <- data.table::fread(checksum_path)
  if (!identical(saved_checksums$md5[match(current_checksums$coder, saved_checksums$coder)], current_checksums$md5)) {
    stop("A coder file changed after adjudication began. Reconcile the change before continuing; the existing adjudication file was preserved.")
  }

  final <- data.table::fread(adjudication_path, na.strings = c("", "NA", "N/A"), encoding = "UTF-8")
  if (!setequal(final$annotation_id, expected_ids)) stop("Adjudication file has unexpected annotation IDs.")
  data.table::setkey(final, annotation_id)
  final <- final[expected_ids]
  final[, stance_label := normalize_stance(stance_label)]
  for (column in frame_columns) final[, (column) := normalize_frame(get(column))]
  valid_stance <- !is.na(final$stance_label) & final$stance_label %in% stance_levels
  complete_final <- valid_stance & rowSums(!is.na(final[, ..frame_columns])) == length(frame_columns)
  if (!all(complete_final)) {
    write_readiness(
      "awaiting_adjudication", nrow(key), nrow(key), nrow(key), 0L,
      sum(!complete_final),
      "Fill the remaining blank or invalid final-label cells in the adjudication file."
    )
    message("Adjudication remains incomplete for ", sum(!complete_final), " items.")
    return(invisible(FALSE))
  }

  validated <- merge(
    final[, c("annotation_id", "tweet_date", label_columns, "adjudication_notes"), with = FALSE],
    key,
    by = "annotation_id", all.x = TRUE, sort = FALSE
  )
  data.table::fwrite(validated, validated_path)
  gate_failures <- metrics[reliability_gate_0_667 == FALSE | is.na(reliability_gate_0_667), measure]
  write_readiness(
    "ready_for_models", nrow(key), nrow(key), nrow(key), 0L, 0L,
    if (length(gate_failures)) paste("Low-reliability measures will be excluded:", paste(gate_failures, collapse = ", ")) else "All measures passed the 0.667 reliability gate."
  )
  message("Validated human labels are ready. Reliability gates remain measure-specific.")
  invisible(TRUE)
}

stance_frame_validation_ready <- run_stance_frame_validation()
