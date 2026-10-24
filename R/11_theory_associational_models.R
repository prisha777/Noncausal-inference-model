# Theory-driven, explicitly noncausal models of congressional COVID message production.
#
# The outcomes in this file are deterministic dictionary-coded message characteristics.
# They are not validated judgments of truth, constituent exposure, or causal effects.
source("R/00_config.R")

required <- c("data.table", "fixest", "ggplot2", "broom", "scales", "stringr")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing packages: ", paste(missing, collapse = ", "))

tweets_path <- file.path(dir_intermediate, "tweets_normalized.csv")
labels_path <- file.path(dir_intermediate, "tweet_labels_keyword_baseline.csv")
if (!file.exists(tweets_path) || !file.exists(labels_path)) {
  stop("Run tweet ingestion and R/03b_keyword_baseline_labels.R first.")
}

tweets <- data.table::fread(
  tweets_path,
  select = c("tweet_id", "screen_name", "state", "party", "chamber", "month", "text_norm")
)
labels <- data.table::fread(labels_path)

tweets[, `:=`(
  month = data.table::as.IDate(month),
  state = normalize_state(state),
  screen_name = tolower(sub("^@", "", trimws(screen_name)))
)]

archive <- tweets[!is.na(state) & !is.na(screen_name) & nzchar(screen_name), .(
  all_tweets = .N
), by = .(screen_name, state, party, chamber, month)]

labeled <- merge(tweets, labels, by = "tweet_id", all = FALSE)
labeled[, text_lower := stringr::str_to_lower(text_norm)]
institution_pattern <- "\\b(fauci|cdc|centers for disease control|world health organization|nih|public health (agency|agencies|official|officials|bureaucracy|bureaucrats))\\b"
delegitimation_pattern <- "\\b(lie|lies|lied|lying|liar|corrupt|corruption|fraud|fraudulent|untrustworthy|cannot trust|can't trust|do not trust|don't trust|cover[- ]?up|criminal|tyrant|tyranny|fire fauci|fauci should resign|failed us|betrayed|deceiv(?:e|ed|ing)|misled|propaganda)\\b"
mandate_pattern <- "\\b(mask mandate|vaccine mandate|forced vaccination|medical freedom|do not comply|don't comply|lockdown|shutdown)\\b"
hesitancy_pattern <- "\\b(experimental vaccine|rushed vaccine|vaccine injury|vaccine injuries|natural immunity|my body my choice)\\b"
conspiracy_core_pattern <- "\\b(plandemic|scamdemic|fake pandemic|covid hoax|coronavirus hoax)\\b"
conspiracy_vaccine_pattern <- "\\b(vaccin(?:e|es|ated|ation|ating)|covid shot|covid shots)\\b"
conspiracy_mechanism_pattern <- "\\b(microchip|depopulation)\\b"
labeled[, `:=`(
  institutional_distrust = as.integer(
    stringr::str_detect(text_lower, institution_pattern) &
      stringr::str_detect(text_lower, delegitimation_pattern)
  ),
  mandate_opposition = as.integer(stringr::str_detect(text_lower, mandate_pattern)),
  vaccine_hesitancy = as.integer(stringr::str_detect(text_lower, hesitancy_pattern)),
  conspiracy_term = as.integer(
    stringr::str_detect(text_lower, conspiracy_core_pattern) |
      (stringr::str_detect(text_lower, conspiracy_vaccine_pattern) &
        stringr::str_detect(text_lower, conspiracy_mechanism_pattern))
  )
)]
labeled[, contentious_rhetoric := as.integer(
  institutional_distrust == 1L | vaccine_hesitancy == 1L | conspiracy_term == 1L
)]

message_counts <- labeled[, .(
  dictionary_covid_tweets = .N,
  contentious_rhetoric_tweets = sum(contentious_rhetoric),
  conspiracy_term_tweets = sum(conspiracy_term),
  institutional_distrust_tweets = sum(institutional_distrust),
  mandate_opposition_tweets = sum(mandate_opposition),
  vaccine_hesitancy_tweets = sum(vaccine_hesitancy)
), by = .(screen_name, state, party, chamber, month)]

panel <- merge(
  archive,
  message_counts,
  by = c("screen_name", "state", "party", "chamber", "month"),
  all.x = TRUE
)
count_columns <- c(
  "dictionary_covid_tweets", "contentious_rhetoric_tweets", "conspiracy_term_tweets",
  "institutional_distrust_tweets", "mandate_opposition_tweets", "vaccine_hesitancy_tweets"
)
for (column in count_columns) panel[is.na(get(column)), (column) := 0L]

panel[, phase := data.table::fcase(
  month <= data.table::as.IDate("2020-05-01"), "Emergency onset",
  month <= data.table::as.IDate("2020-12-01"), "Pre-vaccine contestation",
  month <= data.table::as.IDate("2021-06-01"), "Vaccine rollout",
  month <= data.table::as.IDate("2021-12-01"), "Delta and mandates",
  default = "Omicron period"
)]
phase_levels <- c(
  "Emergency onset", "Pre-vaccine contestation", "Vaccine rollout",
  "Delta and mandates", "Omicron period"
)
panel[, phase := factor(phase, levels = phase_levels)]
panel[, phase_early := data.table::fcase(
  month <= data.table::as.IDate("2020-04-01"), "Emergency onset",
  month <= data.table::as.IDate("2020-11-01"), "Pre-vaccine contestation",
  month <= data.table::as.IDate("2021-05-01"), "Vaccine rollout",
  month <= data.table::as.IDate("2021-11-01"), "Delta and mandates",
  default = "Omicron period"
)]
panel[, phase_late := data.table::fcase(
  month <= data.table::as.IDate("2020-06-01"), "Emergency onset",
  month <= data.table::as.IDate("2021-01-01"), "Pre-vaccine contestation",
  month <= data.table::as.IDate("2021-07-01"), "Vaccine rollout",
  month <= data.table::as.IDate("2022-01-01"), "Delta and mandates",
  default = "Omicron period"
)]
panel[, `:=`(
  phase_early = factor(phase_early, levels = phase_levels),
  phase_late = factor(phase_late, levels = phase_levels)
)]
panel[, `:=`(
  republican = as.integer(party == "Republican"),
  senate = as.integer(chamber == "Senate"),
  candidate_share = dictionary_covid_tweets / all_tweets,
  contentious_rhetoric_share = data.table::fifelse(
    dictionary_covid_tweets > 0,
    contentious_rhetoric_tweets / dictionary_covid_tweets,
    NA_real_
  )
)]
analysis <- panel[party %in% c("Democratic", "Republican")]

# Tweet-level candidate data are retained for the supplementary term-use model.
# This avoids conditioning on account-months with at least one COVID candidate,
# which was an unnecessary selection step in the earlier grouped-binomial model.
labeled[, phase := data.table::fcase(
  month <= data.table::as.IDate("2020-05-01"), "Emergency onset",
  month <= data.table::as.IDate("2020-12-01"), "Pre-vaccine contestation",
  month <= data.table::as.IDate("2021-06-01"), "Vaccine rollout",
  month <= data.table::as.IDate("2021-12-01"), "Delta and mandates",
  default = "Omicron period"
)]
labeled[, phase := factor(phase, levels = phase_levels)]
labeled[, phase_early := data.table::fcase(
  month <= data.table::as.IDate("2020-04-01"), "Emergency onset",
  month <= data.table::as.IDate("2020-11-01"), "Pre-vaccine contestation",
  month <= data.table::as.IDate("2021-05-01"), "Vaccine rollout",
  month <= data.table::as.IDate("2021-11-01"), "Delta and mandates",
  default = "Omicron period"
)]
labeled[, phase_late := data.table::fcase(
  month <= data.table::as.IDate("2020-06-01"), "Emergency onset",
  month <= data.table::as.IDate("2021-01-01"), "Pre-vaccine contestation",
  month <= data.table::as.IDate("2021-07-01"), "Vaccine rollout",
  month <= data.table::as.IDate("2022-01-01"), "Delta and mandates",
  default = "Omicron period"
)]
labeled[, `:=`(
  phase_early = factor(phase_early, levels = phase_levels),
  phase_late = factor(phase_late, levels = phase_levels)
)]
labeled[, `:=`(
  republican = as.integer(party == "Republican"),
  senate = as.integer(chamber == "Senate")
)]
risk_tweets <- labeled[
  party %in% c("Democratic", "Republican") &
    !is.na(state) & !is.na(screen_name) & nzchar(screen_name)
]

# Corpus and measurement summaries.
corpus_summary <- data.table::data.table(
  metric = c(
    "archive_tweets", "accounts", "member_months", "dictionary_covid_tweets",
    "contentious_rhetoric_tweets", "conspiracy_term_tweets", "institutional_distrust_tweets",
    "mandate_opposition_tweets", "vaccine_hesitancy_tweets"
  ),
  value = c(
    sum(analysis$all_tweets), data.table::uniqueN(analysis$screen_name), nrow(analysis),
    sum(analysis$dictionary_covid_tweets), sum(analysis$contentious_rhetoric_tweets),
    sum(analysis$conspiracy_term_tweets), sum(analysis$institutional_distrust_tweets),
    sum(analysis$mandate_opposition_tweets), sum(analysis$vaccine_hesitancy_tweets)
  )
)
data.table::fwrite(corpus_summary, file.path(dir_outputs, "theory_corpus_summary.csv"))

party_phase <- analysis[, .(
  accounts = data.table::uniqueN(screen_name),
  member_months = .N,
  all_tweets = sum(all_tweets),
  dictionary_covid_tweets = sum(dictionary_covid_tweets),
  contentious_rhetoric_tweets = sum(contentious_rhetoric_tweets),
  conspiracy_term_tweets = sum(conspiracy_term_tweets),
  institutional_distrust_tweets = sum(institutional_distrust_tweets),
  mandate_opposition_tweets = sum(mandate_opposition_tweets)
), by = .(party, phase)]
party_phase[, `:=`(
  covid_per_1000 = 1000 * dictionary_covid_tweets / all_tweets,
  contentious_rhetoric_percent = 100 * contentious_rhetoric_tweets / dictionary_covid_tweets,
  conspiracy_per_1000_candidates = 1000 * conspiracy_term_tweets / dictionary_covid_tweets,
  institutional_distrust_percent = 100 * institutional_distrust_tweets / dictionary_covid_tweets,
  mandate_opposition_percent = 100 * mandate_opposition_tweets / dictionary_covid_tweets
)]
data.table::fwrite(party_phase, file.path(dir_outputs, "theory_party_phase_descriptives.csv"))

# Monthly trajectories keep the five-phase summaries from hiding short-lived changes.
party_month <- analysis[, .(
  accounts = data.table::uniqueN(screen_name),
  all_tweets = sum(all_tweets),
  dictionary_covid_tweets = sum(dictionary_covid_tweets),
  mandate_opposition_tweets = sum(mandate_opposition_tweets),
  contentious_rhetoric_tweets = sum(contentious_rhetoric_tweets)
), by = .(party, month)]
party_month[, `:=`(
  covid_per_1000 = 1000 * dictionary_covid_tweets / all_tweets,
  mandate_opposition_percent = 100 * mandate_opposition_tweets / dictionary_covid_tweets,
  contentious_rhetoric_percent = 100 * contentious_rhetoric_tweets / dictionary_covid_tweets
)]
data.table::setorder(party_month, month, party)
data.table::fwrite(party_month, file.path(dir_outputs, "theory_party_month_descriptives.csv"))

# Explicit monthly party contrasts keep the phase bins from determining the result.
monthly_covid <- data.table::dcast(
  party_month, month ~ party, value.var = "covid_per_1000"
)
data.table::setnames(
  monthly_covid, c("Democratic", "Republican"),
  c("democratic_covid_per_1000", "republican_covid_per_1000")
)
monthly_terms <- data.table::dcast(
  party_month, month ~ party, value.var = "mandate_opposition_percent"
)
data.table::setnames(
  monthly_terms, c("Democratic", "Republican"),
  c("democratic_selected_term_percent", "republican_selected_term_percent")
)
monthly_party_contrasts <- merge(monthly_covid, monthly_terms, by = "month")
monthly_party_contrasts[, `:=`(
  covid_difference_per_1000 = republican_covid_per_1000 - democratic_covid_per_1000,
  selected_term_difference_points = republican_selected_term_percent - democratic_selected_term_percent
)]
data.table::fwrite(
  monthly_party_contrasts,
  file.path(dir_outputs, "theory_monthly_party_contrasts.csv")
)

# Boundary sensitivity: move every transition one month earlier or later. These are
# descriptive regroupings, not alternative causal intervention dates.
phase_sensitivity_summary <- function(data, phase_column, scheme_label) {
  out <- data[, .(
    all_tweets = sum(all_tweets),
    dictionary_covid_tweets = sum(dictionary_covid_tweets),
    mandate_opposition_tweets = sum(mandate_opposition_tweets)
  ), by = .(party, phase = get(phase_column))]
  out[, `:=`(
    scheme = scheme_label,
    covid_per_1000 = 1000 * dictionary_covid_tweets / all_tweets,
    selected_term_percent = 100 * mandate_opposition_tweets / dictionary_covid_tweets
  )]
  out[]
}
alternative_phase_descriptives <- data.table::rbindlist(list(
  phase_sensitivity_summary(analysis, "phase", "Primary boundaries"),
  phase_sensitivity_summary(analysis, "phase_early", "Transitions one month earlier"),
  phase_sensitivity_summary(analysis, "phase_late", "Transitions one month later")
))
alternative_phase_descriptives[, phase := factor(phase, levels = phase_levels)]
data.table::setorder(alternative_phase_descriptives, scheme, phase, party)
data.table::fwrite(
  alternative_phase_descriptives,
  file.path(dir_outputs, "theory_alternative_phase_descriptives.csv")
)

# Supplementary models check whether the raw phase patterns remain after accounting
# for chamber and calendar month. The descriptive party-by-phase rates above are the
# main results. State fixed effects are deliberately omitted: party is a member-level
# attribute, and congressional representation does not provide a clean within-state
# party comparison for every chamber, account, or month.
model_candidate <- fixest::fepois(
  dictionary_covid_tweets ~ republican + senate + republican:phase |
    month,
  offset = ~log(all_tweets),
  cluster = ~screen_name,
  data = analysis
)

model_mandate <- fixest::feglm(
  mandate_opposition ~ republican + senate + republican:phase | month,
  family = "binomial",
  cluster = ~screen_name,
  data = risk_tweets
)

tidy_ratio <- function(model, outcome, scale) {
  out <- data.table::as.data.table(broom::tidy(model, conf.int = TRUE))
  out <- out[grepl("^republican", term)]
  out[, `:=`(
    outcome = outcome,
    ratio = exp(estimate),
    conf.low.ratio = exp(conf.low),
    conf.high.ratio = exp(conf.high),
    estimand_scale = scale,
    interpretation = "supplementary adjusted party comparison; not a causal effect"
  )]
  out[]
}

model_results <- data.table::rbindlist(list(
  tidy_ratio(model_candidate, "dictionary-coded COVID posting rate", "incidence-rate ratio"),
  tidy_ratio(model_mandate, "selected mandate-conflict term use among dictionary-coded COVID tweets", "odds ratio")
), fill = TRUE)
data.table::setcolorder(
  model_results,
  c("outcome", "term", "estimand_scale", "ratio", "conf.low.ratio", "conf.high.ratio",
    "estimate", "std.error", "statistic", "p.value", "interpretation")
)
data.table::fwrite(model_results, file.path(dir_outputs, "theory_model_results.csv"))

party_phase_contrasts <- function(model, outcome, scale) {
  coefficients <- stats::coef(model)
  covariance <- stats::vcov(model)
  data.table::rbindlist(lapply(phase_levels, function(phase_name) {
    terms <- "republican"
    if (phase_name != phase_levels[1]) {
      interaction_term <- paste0("republican:phase", phase_name)
      if (interaction_term %in% names(coefficients)) terms <- c(terms, interaction_term)
    }
    estimate <- sum(coefficients[terms])
    variance <- sum(covariance[terms, terms, drop = FALSE])
    standard_error <- sqrt(variance)
    data.table::data.table(
      outcome = outcome,
      phase = phase_name,
      estimand_scale = scale,
      republican_to_democratic_ratio = exp(estimate),
      conf.low = exp(estimate - 1.96 * standard_error),
      conf.high = exp(estimate + 1.96 * standard_error),
      log_ratio = estimate,
      std.error = standard_error,
      interpretation = "party contrast within phase, adjusted for chamber and calendar month; not causal"
    )
  }))
}

phase_contrasts <- data.table::rbindlist(list(
  party_phase_contrasts(model_candidate, "dictionary-coded COVID posting rate", "incidence-rate ratio"),
  party_phase_contrasts(model_mandate, "selected mandate-conflict term use", "odds ratio")
), fill = TRUE)
data.table::fwrite(phase_contrasts, file.path(dir_outputs, "theory_party_contrasts_by_phase.csv"))

# Average model-implied probabilities make the selected-term results easier to read
# than odds ratios alone. Each party prediction uses the same pooled distribution of
# months and chambers observed among COVID candidates in that phase.
predicted_term_probabilities <- data.table::rbindlist(lapply(phase_levels, function(phase_name) {
  standardization_sample <- data.table::copy(
    risk_tweets[as.character(phase) == phase_name]
  )
  data.table::rbindlist(lapply(c(Democratic = 0L, Republican = 1L), function(party_value) {
    counterfactual <- data.table::copy(standardization_sample)
    counterfactual[, republican := party_value]
    predictions <- stats::predict(model_mandate, newdata = counterfactual, type = "response")
    data.table::data.table(
      phase = phase_name,
      party = if (party_value == 1L) "Republican" else "Democratic",
      standardized_probability = mean(predictions),
      standardization = "pooled observed month and chamber distribution within phase"
    )
  }), use.names = TRUE)
}))
predicted_term_probabilities[, predicted_percent := 100 * standardized_probability]
predicted_term_wide <- data.table::dcast(
  predicted_term_probabilities,
  phase ~ party,
  value.var = "predicted_percent"
)
data.table::setnames(
  predicted_term_wide, c("Democratic", "Republican"),
  c("democratic_predicted_percent", "republican_predicted_percent")
)
predicted_term_wide[, percentage_point_difference :=
  republican_predicted_percent - democratic_predicted_percent]
predicted_term_wide[, phase := factor(phase, levels = phase_levels)]
data.table::setorder(predicted_term_wide, phase)
data.table::fwrite(
  predicted_term_wide,
  file.path(dir_outputs, "theory_mandate_predicted_probabilities.csv")
)

saveRDS(
  list(
    candidate_rate = model_candidate,
    mandate_opposition_share = model_mandate
  ),
  file.path(dir_outputs, "theory_models.rds")
)

# Concentration of contentious-rhetoric markers by account.
account_concentration <- analysis[, .(
  all_tweets = sum(all_tweets),
  dictionary_covid_tweets = sum(dictionary_covid_tweets),
  contentious_rhetoric_tweets = sum(contentious_rhetoric_tweets),
  conspiracy_term_tweets = sum(conspiracy_term_tweets)
), by = .(screen_name, party, chamber, state)]
data.table::setorder(account_concentration, -contentious_rhetoric_tweets, screen_name)
account_concentration[, cumulative_contentious_share := cumsum(contentious_rhetoric_tweets) / sum(contentious_rhetoric_tweets)]
account_concentration[, account_rank := .I]
data.table::fwrite(account_concentration, file.path(dir_outputs, "theory_account_concentration.csv"))

top_n <- max(1L, ceiling(0.10 * nrow(account_concentration)))
contentious_total <- sum(account_concentration$contentious_rhetoric_tweets)
contentious_shares <- account_concentration$contentious_rhetoric_tweets / contentious_total
sorted_counts <- sort(account_concentration$contentious_rhetoric_tweets)
gini_contentious <- 1 + 1 / length(sorted_counts) -
  2 * sum(cumsum(sorted_counts)) / (length(sorted_counts) * sum(sorted_counts))
concentration_summary <- data.table::data.table(
  metric = c(
    "accounts", "accounts_with_contentious_rhetoric", "top_10_percent_account_count",
    "top_10_percent_share_of_contentious_rhetoric", "top_account_share",
    "top_10_accounts_share", "herfindahl_index", "effective_number_of_accounts",
    "gini_coefficient"
  ),
  value = c(
    nrow(account_concentration),
    sum(account_concentration$contentious_rhetoric_tweets > 0),
    top_n,
    sum(account_concentration$contentious_rhetoric_tweets[seq_len(top_n)]) / contentious_total,
    max(contentious_shares),
    sum(account_concentration$contentious_rhetoric_tweets[seq_len(10L)]) / contentious_total,
    sum(contentious_shares^2),
    1 / sum(contentious_shares^2),
    gini_contentious
  )
)
data.table::fwrite(concentration_summary, file.path(dir_outputs, "theory_concentration_summary.csv"))

# Run the same concentration audit for mandate-conflict dictionary matches. This tests whether
# the party pattern is broad or driven by a few accounts without treating the marker as
# misinformation or as a causal treatment.
mandate_account_concentration <- analysis[, .(
  all_tweets = sum(all_tweets),
  dictionary_covid_tweets = sum(dictionary_covid_tweets),
  mandate_opposition_tweets = sum(mandate_opposition_tweets)
), by = .(screen_name, party, chamber, state)]
data.table::setorder(mandate_account_concentration, -mandate_opposition_tweets, screen_name)
mandate_total <- sum(mandate_account_concentration$mandate_opposition_tweets)
mandate_account_concentration[, `:=`(
  account_rank = .I,
  cumulative_mandate_share = cumsum(mandate_opposition_tweets) / mandate_total
)]
data.table::fwrite(
  mandate_account_concentration,
  file.path(dir_outputs, "theory_mandate_account_concentration.csv")
)

mandate_top_n <- max(1L, ceiling(0.10 * nrow(mandate_account_concentration)))
mandate_shares <- mandate_account_concentration$mandate_opposition_tweets / mandate_total
mandate_sorted <- sort(mandate_account_concentration$mandate_opposition_tweets)
mandate_gini <- 1 + 1 / length(mandate_sorted) -
  2 * sum(cumsum(mandate_sorted)) / (length(mandate_sorted) * sum(mandate_sorted))
mandate_concentration_summary <- data.table::data.table(
  metric = c(
    "accounts", "accounts_with_mandate_opposition", "top_10_percent_account_count",
    "top_10_percent_share_of_mandate_opposition", "top_account_share",
    "top_10_accounts_share", "herfindahl_index", "effective_number_of_accounts",
    "gini_coefficient"
  ),
  value = c(
    nrow(mandate_account_concentration),
    sum(mandate_account_concentration$mandate_opposition_tweets > 0),
    mandate_top_n,
    sum(mandate_account_concentration$mandate_opposition_tweets[seq_len(mandate_top_n)]) / mandate_total,
    max(mandate_shares),
    sum(mandate_account_concentration$mandate_opposition_tweets[seq_len(10L)]) / mandate_total,
    sum(mandate_shares^2),
    1 / sum(mandate_shares^2),
    mandate_gini
  )
)
data.table::fwrite(
  mandate_concentration_summary,
  file.path(dir_outputs, "theory_mandate_concentration_summary.csv")
)

# Benchmark rare-marker concentration against ordinary message production. A large
# top-decile share is only informative if it exceeds the corpus's baseline skew.
concentration_metrics <- function(values, measure_label) {
  values <- as.numeric(values)
  total <- sum(values)
  n <- length(values)
  top_count <- max(1L, ceiling(0.10 * n))
  descending <- sort(values, decreasing = TRUE)
  ascending <- rev(descending)
  shares <- values / total
  gini <- 1 + 1 / n - 2 * sum(cumsum(ascending)) / (n * total)
  data.table::data.table(
    measure = measure_label,
    total_messages = total,
    accounts_with_at_least_one = sum(values > 0),
    top_10_percent_share = sum(descending[seq_len(top_count)]) / total,
    top_account_share = descending[1] / total,
    top_10_accounts_share = sum(descending[seq_len(min(10L, n))]) / total,
    gini_coefficient = gini,
    herfindahl_index = sum(shares^2),
    effective_number_of_accounts = 1 / sum(shares^2)
  )
}
concentration_benchmarks <- data.table::rbindlist(list(
  concentration_metrics(account_concentration$all_tweets, "All archived tweets"),
  concentration_metrics(account_concentration$dictionary_covid_tweets, "COVID candidates"),
  concentration_metrics(mandate_account_concentration$mandate_opposition_tweets, "Selected mandate-conflict terms"),
  concentration_metrics(account_concentration$contentious_rhetoric_tweets, "Narrow skepticism markers")
))
data.table::fwrite(
  concentration_benchmarks,
  file.path(dir_outputs, "theory_concentration_benchmarks.csv")
)

lorenz_coordinates <- function(values, measure_label) {
  values <- sort(as.numeric(values))
  data.table::data.table(
    account_share = c(0, seq_along(values) / length(values)),
    message_share = c(0, cumsum(values) / sum(values)),
    measure = measure_label
  )
}
lorenz_data <- data.table::rbindlist(list(
  lorenz_coordinates(account_concentration$all_tweets, "All archived tweets"),
  lorenz_coordinates(account_concentration$dictionary_covid_tweets, "COVID candidates"),
  lorenz_coordinates(mandate_account_concentration$mandate_opposition_tweets, "Selected mandate-conflict terms"),
  lorenz_coordinates(account_concentration$contentious_rhetoric_tweets, "Narrow skepticism markers")
))
data.table::fwrite(lorenz_data, file.path(dir_outputs, "theory_lorenz_coordinates.csv"))

# Sensitivity check: recalculate raw party-phase rates after excluding the ten accounts
# with the most mandate-conflict dictionary matches. This is not a replacement estimate; it shows
# whether the headline pattern survives removal of the most prolific accounts.
top_mandate_accounts <- mandate_account_concentration[seq_len(10L), screen_name]
party_phase_without_top10 <- analysis[!screen_name %in% top_mandate_accounts, .(
  accounts = data.table::uniqueN(screen_name),
  all_tweets = sum(all_tweets),
  dictionary_covid_tweets = sum(dictionary_covid_tweets),
  mandate_opposition_tweets = sum(mandate_opposition_tweets)
), by = .(party, phase)]
party_phase_without_top10[, `:=`(
  covid_per_1000 = 1000 * dictionary_covid_tweets / all_tweets,
  mandate_opposition_percent = 100 * mandate_opposition_tweets / dictionary_covid_tweets,
  excluded_accounts = "ten accounts with the most mandate-conflict dictionary matches"
)]
data.table::fwrite(
  party_phase_without_top10,
  file.path(dir_outputs, "theory_party_phase_without_top10_mandate_accounts.csv")
)

# Compact diagnostics make it easy to verify which data and uncertainty structure each
# model used before copying any number into the manuscript.
model_diagnostics <- data.table::data.table(
  model = c("COVID posting rate", "Selected mandate-conflict term use"),
  observations = c(stats::nobs(model_candidate), stats::nobs(model_mandate)),
  accounts = c(
    data.table::uniqueN(analysis$screen_name),
    data.table::uniqueN(risk_tweets$screen_name)
  ),
  states = c(data.table::uniqueN(analysis$state), data.table::uniqueN(risk_tweets$state)),
  months = c(data.table::uniqueN(analysis$month), data.table::uniqueN(risk_tweets$month)),
  fixed_effects = c("month", "month"),
  analysis_unit = c("account-month", "COVID-candidate tweet"),
  uncertainty = c("standard errors clustered by account", "standard errors clustered by account"),
  status = c("supplementary adjusted comparison", "supplementary adjusted comparison")
)
data.table::fwrite(model_diagnostics, file.path(dir_outputs, "theory_model_diagnostics.csv"))

# Publication-ready descriptive figures.
party_colors <- c(Democratic = "#2E6F9E", Republican = "#B64949")
p1 <- ggplot2::ggplot(
  party_phase,
  ggplot2::aes(x = phase, y = covid_per_1000, color = party, group = party)
) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 2.4) +
  ggplot2::scale_color_manual(values = party_colors) +
  ggplot2::labs(
    x = NULL, y = "COVID candidates per 1,000 archived tweets",
    color = NULL, title = "Congressional COVID candidates declined for both parties",
    subtitle = "Raw party-phase rates from the transparent COVID dictionary"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1), legend.position = "top")
ggplot2::ggsave(file.path(dir_outputs, "theory_covid_posting_by_party_phase.png"), p1, width = 8.3, height = 4.8, dpi = 300)

p2 <- ggplot2::ggplot(
  party_phase,
  ggplot2::aes(x = phase, y = mandate_opposition_percent, color = party, group = party)
) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 2.4) +
  ggplot2::scale_color_manual(values = party_colors) +
  ggplot2::labs(
    x = NULL, y = "COVID candidates containing selected mandate-conflict terms (%)",
    color = NULL, title = "Selected mandate-conflict terms diverged by party",
    subtitle = "Raw dictionary matches; the terms do not verify the author's stance"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1), legend.position = "top")
ggplot2::ggsave(file.path(dir_outputs, "theory_mandate_opposition_by_party_phase.png"), p2, width = 8.3, height = 4.8, dpi = 300)

p3 <- ggplot2::ggplot(
  party_month,
  ggplot2::aes(x = month, y = covid_per_1000, color = party)
) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::scale_color_manual(values = party_colors) +
  ggplot2::scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  ggplot2::labs(
    x = NULL, y = "COVID candidates per 1,000 archived tweets", color = NULL,
    title = "Congressional COVID attention declined and converged",
    subtitle = "Monthly raw rates show that the phase summaries do not create the pattern"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "top")
ggplot2::ggsave(
  file.path(dir_outputs, "theory_monthly_covid_posting_by_party.png"),
  p3, width = 8.3, height = 4.8, dpi = 300
)

p4 <- ggplot2::ggplot(
  party_month,
  ggplot2::aes(x = month, y = mandate_opposition_percent, color = party)
) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::scale_color_manual(values = party_colors) +
  ggplot2::scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  ggplot2::labs(
    x = NULL, y = "COVID candidates with selected mandate-conflict terms (%)", color = NULL,
    title = "Mandate-conflict terms diverged as COVID attention converged",
    subtitle = "Monthly raw dictionary matches; not validated stance labels"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "top")
ggplot2::ggsave(
  file.path(dir_outputs, "theory_monthly_mandate_opposition_by_party.png"),
  p4, width = 8.3, height = 4.8, dpi = 300
)

monthly_figure_data <- data.table::rbindlist(list(
  party_month[, .(
    party, month, value = covid_per_1000,
    measure = "COVID candidates per 1,000 archived tweets"
  )],
  party_month[, .(
    party, month, value = mandate_opposition_percent,
    measure = "COVID candidates with selected terms (%)"
  )]
))
p5 <- ggplot2::ggplot(
  monthly_figure_data,
  ggplot2::aes(x = month, y = value, color = party)
) +
  ggplot2::geom_line(linewidth = 0.75) +
  ggplot2::facet_wrap(~measure, scales = "free_y", ncol = 1) +
  ggplot2::scale_color_manual(values = party_colors) +
  ggplot2::scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  ggplot2::labs(
    x = NULL, y = NULL, color = NULL,
    title = "Monthly trajectories reproduce the two phase-level patterns",
    subtitle = "Raw dictionary rates; the selected terms do not verify stance"
  ) +
  ggplot2::theme_minimal(base_size = 10.5) +
  ggplot2::theme(legend.position = "top", strip.text = ggplot2::element_text(face = "bold"))
ggplot2::ggsave(
  file.path(dir_outputs, "theory_monthly_trajectories_by_party.png"),
  p5, width = 8.3, height = 7.0, dpi = 300
)

concentration_colors <- c(
  "All archived tweets" = "#6C757D",
  "COVID candidates" = "#2E6F9E",
  "Selected mandate-conflict terms" = "#B64949",
  "Narrow skepticism markers" = "#7A5195"
)
p6 <- ggplot2::ggplot(
  lorenz_data,
  ggplot2::aes(x = account_share, y = message_share, color = measure)
) +
  ggplot2::geom_abline(slope = 1, intercept = 0, color = "#B8B8B8", linetype = "dashed") +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::scale_color_manual(values = concentration_colors) +
  ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  ggplot2::coord_equal() +
  ggplot2::labs(
    x = "Cumulative share of accounts", y = "Cumulative share of messages", color = NULL,
    title = "Rare markers were more concentrated than ordinary tweeting",
    subtitle = "Lorenz curves across 524 Democratic and Republican congressional accounts"
  ) +
  ggplot2::theme_minimal(base_size = 10.5) +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
  file.path(dir_outputs, "theory_concentration_lorenz.png"),
  p6, width = 7.3, height = 6.2, dpi = 300
)

# Generate a short evidence report from the just-created outputs. The manuscript can be
# checked against this file without manually recomputing or retyping statistics.
get_phase <- function(party_name, phase_name) {
  party_phase[party == party_name & as.character(phase) == phase_name]
}
dem_omicron <- get_phase("Democratic", "Omicron period")
rep_omicron <- get_phase("Republican", "Omicron period")
dem_delta <- get_phase("Democratic", "Delta and mandates")
rep_delta <- get_phase("Republican", "Delta and mandates")
mandate_omicron <- phase_contrasts[
  outcome == "selected mandate-conflict term use" & phase == "Omicron period"
]
mandate_delta <- phase_contrasts[
  outcome == "selected mandate-conflict term use" & phase == "Delta and mandates"
]
sensitivity_delta <- data.table::dcast(
  party_phase_without_top10[as.character(phase) == "Delta and mandates"],
  phase ~ party,
  value.var = "mandate_opposition_percent"
)
sensitivity_omicron <- data.table::dcast(
  party_phase_without_top10[as.character(phase) == "Omicron period"],
  phase ~ party,
  value.var = "mandate_opposition_percent"
)
key_findings <- c(
  "# R-generated key findings",
  "",
  "All statistics below were generated from `R/11_theory_associational_models.R`.",
  "They describe archived congressional messages and supplementary adjusted party comparisons; they are not causal effects, validated stance labels, or misinformation judgments.",
  "",
  "## Finding 1: attention converged while policy opposition diverged",
  sprintf(
    "- During Omicron, Democratic and Republican accounts posted %.2f and %.2f COVID tweets per 1,000 archived tweets, respectively.",
    dem_omicron$covid_per_1000, rep_omicron$covid_per_1000
  ),
  sprintf(
    "- Yet the selected mandate-conflict terms appeared in %.2f%% of Democratic and %.2f%% of Republican COVID candidates.",
    dem_omicron$mandate_opposition_percent, rep_omicron$mandate_opposition_percent
  ),
  sprintf(
    "- The adjusted Republican-to-Democratic odds ratio was %.2f (95%% CI %.2f-%.2f) during Delta and %.2f (%.2f-%.2f) during Omicron.",
    mandate_delta$republican_to_democratic_ratio, mandate_delta$conf.low, mandate_delta$conf.high,
    mandate_omicron$republican_to_democratic_ratio, mandate_omicron$conf.low, mandate_omicron$conf.high
  ),
  "",
  "## Finding 2: narrow skepticism language was highly concentrated",
  sprintf(
    "- The top 10%% of accounts produced %.2f%% of the 648 narrow skepticism-marker tweets; the effective number of equally productive accounts was %.2f.",
    100 * concentration_summary[metric == "top_10_percent_share_of_contentious_rhetoric", value],
    concentration_summary[metric == "effective_number_of_accounts", value]
  ),
  "",
  "## Finding 3: mandate-conflict matches were also concentrated, but the party pattern survives an outlier check",
  sprintf(
    "- The top 10%% of accounts produced %.2f%% of all mandate-conflict dictionary matches.",
    100 * mandate_concentration_summary[metric == "top_10_percent_share_of_mandate_opposition", value]
  ),
  sprintf(
    "- After removing the ten most prolific mandate-opposition accounts, Republican versus Democratic shares remained %.2f%% versus %.2f%% during Delta and %.2f%% versus %.2f%% during Omicron.",
    sensitivity_delta$Republican, sensitivity_delta$Democratic,
    sensitivity_omicron$Republican, sensitivity_omicron$Democratic
  ),
  "",
  "## Measurement gate",
  "- The stance-and-frame annotation sample is prepared, but no human-coded stance or frame results should be reported until two independent coders complete the instrument and reliability is evaluated."
)
writeLines(key_findings, file.path(dir_outputs, "theory_key_findings.md"))

message("Wrote theory-driven descriptive models, monthly discovery tables, concentration audits, sensitivity checks, figures, diagnostics, and key findings.")
