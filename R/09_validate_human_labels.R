# Score blinded human labels against the current tweet-label file.
# Human coders should never see this script's key or model labels.
source("R/00_config.R")

sample_path <- file.path(dir_intermediate, "validation", "human_coding_sample.csv")
key_path <- file.path(dir_intermediate, "validation", "human_coding_key.csv")
labels_path <- file.path(dir_intermediate, "tweet_labels_normalized.csv")
if (!file.exists(sample_path) || !file.exists(key_path) || !file.exists(labels_path)) {
  stop("Need the human-coding sample, coding key, and current normalized labels.")
}

human <- data.table::fread(sample_path, na.strings = c("", "NA", "N/A"))
key <- data.table::fread(key_path)
labels <- data.table::fread(labels_path)

# Rejoin predictions at scoring time, so the blinded sample can assess the
# returned structured labels rather than permanently scoring the old baseline.
key[, tweet_id := as.character(tweet_id)]
labels[, tweet_id := as.character(tweet_id)]
model <- labels[, .(
  tweet_id,
  model_is_covid_related = is_covid_related,
  model_category = category,
  model_misinformation_score = misinformation_score,
  model_label_source = label_source
)]
key_meta <- key[, .(validation_id, tweet_id, party, state, period, stratum, stratum_population, sample_weight)]
d <- merge(human, key_meta, by = "validation_id", all.x = TRUE, sort = FALSE)
d <- merge(d, model, by = "tweet_id", all.x = TRUE, sort = FALSE)

human_misinfo <- suppressWarnings(as.integer(d$human_misinformation))
human_severity <- suppressWarnings(as.numeric(d$human_severity))
d[, human_misinformation := human_misinfo]
d[, human_severity := human_severity]
d[, model_misinformation_score := suppressWarnings(as.numeric(model_misinformation_score))]

bin <- d[human_misinformation %in% c(0L, 1L) & is.finite(model_misinformation_score)]
sev <- d[is.finite(human_severity) & human_severity >= 0 & human_severity <= 5 & is.finite(model_misinformation_score)]
binary_complete <- nrow(bin) == nrow(d)
severity_complete <- nrow(sev) == nrow(d)
status <- if (binary_complete && severity_complete) "complete" else if (nrow(bin) || nrow(sev)) "partial" else "awaiting_human_labels"
readiness <- data.table::data.table(
  status = status,
  total_sample = nrow(d),
  binary_labels_completed = nrow(bin),
  severity_labels_completed = nrow(sev),
  model_labels_available = sum(is.finite(d$model_misinformation_score)),
  model_label_sources = paste(unique(stats::na.omit(d$model_label_source)), collapse = ";")
)
data.table::fwrite(readiness, file.path(dir_outputs, "label_validation_readiness.csv"))

if (!nrow(bin)) {
  message("Awaiting human labels in data/intermediate/validation/human_coding_sample.csv")
} else {
  bin[, predicted := as.integer(model_misinformation_score >= 3)]
  bin[, w := sample_weight]
  safe_div <- function(x, y) if (is.finite(y) && y > 0) x / y else NA_real_
  binary_metrics <- function(x) {
    tp <- x[predicted == 1 & human_misinformation == 1, sum(w)]
    fp <- x[predicted == 1 & human_misinformation == 0, sum(w)]
    fn <- x[predicted == 0 & human_misinformation == 1, sum(w)]
    tn <- x[predicted == 0 & human_misinformation == 0, sum(w)]
    data.table::data.table(
      precision = safe_div(tp, tp + fp), recall = safe_div(tp, tp + fn),
      specificity = safe_div(tn, tn + fp), f1 = safe_div(2 * tp, 2 * tp + fp + fn),
      accuracy = safe_div(tp + tn, tp + tn + fp + fn)
    )
  }
  m <- data.table::melt(binary_metrics(bin), variable.name = "metric", value.name = "value")
  m[, labels_completed := nrow(bin)]

  severity_metrics <- function(x) {
    if (nrow(x) < 2) return(data.table::data.table(exact = NA_real_, mae = NA_real_, rho = NA_real_, qwk = NA_real_))
    exact <- weighted.mean(as.numeric(x$human_severity == x$model_misinformation_score), x$w)
    mae <- weighted.mean(abs(x$human_severity - x$model_misinformation_score), x$w)
    rho <- suppressWarnings(stats::cor(x$human_severity, x$model_misinformation_score, method = "spearman"))
    levels <- 0:5
    observed <- outer(levels, levels, Vectorize(function(i, j) sum(x[human_severity == i & model_misinformation_score == j, w])))
    agreement_weight <- 1 - outer(levels, levels, "-")^2 / 25
    expected <- outer(rowSums(observed), colSums(observed)) / sum(observed)
    qwk <- (sum(agreement_weight * observed) / sum(observed) - sum(agreement_weight * expected) / sum(expected)) /
      (1 - sum(agreement_weight * expected) / sum(expected))
    data.table::data.table(exact = exact, mae = mae, rho = rho, qwk = qwk)
  }
  if (nrow(sev)) {
    sev[, w := sample_weight]
    sm <- data.table::melt(severity_metrics(sev), variable.name = "metric", value.name = "value")
    sm[, `:=`(metric = paste0("severity_", metric), labels_completed = nrow(sev))]
    m <- data.table::rbindlist(list(m, sm))
  }
  data.table::fwrite(m, file.path(dir_outputs, "label_validation_metrics.csv"))

  party_bin <- bin[, binary_metrics(.SD), by = party]
  party_bin <- data.table::melt(party_bin, id.vars = "party", variable.name = "metric", value.name = "value")
  party_bin[, labels_completed := bin[party == .BY$party, .N], by = party]
  party_sev <- if (nrow(sev)) {
    sev[, severity_metrics(.SD), by = party]
  } else data.table::data.table(party = character())
  if (nrow(party_sev)) {
    party_sev <- data.table::melt(party_sev, id.vars = "party", variable.name = "metric", value.name = "value")
    party_sev[, `:=`(metric = paste0("severity_", metric), labels_completed = sev[party == .BY$party, .N]), by = party]
  }
  data.table::fwrite(data.table::rbindlist(list(party_bin, party_sev), fill = TRUE), file.path(dir_outputs, "label_validation_by_party.csv"))
  message("Wrote weighted binary and severity validation reports. Status: ", status)
}
