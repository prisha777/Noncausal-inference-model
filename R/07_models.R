# Descriptive and associational analyses for the state-month communication panel.
# This script estimates conditional statistical associations only; it does not
# use quasi-experimental estimators or temporal-ordering transformations.
source("R/00_config.R")
source("R/10_audit_label_readiness.R")

panel_path <- file.path(dir_processed, "state_month_associational_panel.csv")
readiness_path <- file.path(dir_outputs, "association_model_readiness.csv")
primary_indicator_names <- c("cdc_covid_monthly__log1p_monthly_cases", "cdc_covid_monthly__log1p_monthly_deaths")

if (!file.exists(panel_path)) stop("Missing state_month_associational_panel.csv. Run R/04_build_communication_measures.R and R/06_make_panel.R first.")
if (!requireNamespace("fixest", quietly = TRUE) || !requireNamespace("broom", quietly = TRUE)) stop("R/07_models.R requires fixest and broom.")

allow_baseline_diagnostics <- identical(Sys.getenv("ALLOW_BASELINE_DESCRIPTIVE_DIAGNOSTICS"), "1")
if (!label_measurement_ready_for_descriptive_reporting && !allow_baseline_diagnostics) {
  stop("Tweet labels are not ready for substantive descriptive reporting. See ", label_measurement_readiness_audit_path, ". Complete structured labels, blinded validation, and review sign-off. For a pipeline-only baseline diagnostic, set ALLOW_BASELINE_DESCRIPTIVE_DIAGNOSTICS=1.")
}
result_status <- if (label_measurement_ready_for_descriptive_reporting) "validated_label_descriptive_association" else "keyword_baseline_diagnostic_not_for_interpretation"

panel <- data.table::fread(panel_path)
panel[, month := data.table::as.IDate(month)]
structural_cols <- c("state", "month", "members_observed", "labeled_tweets", "covid_related_tweets", "high_severity_tweets", "severity_sum", "mean_member_severity", "mean_tweet_severity", "high_severity_share", "republican_member_share", "members_with_retweet_counts", "members_with_favorite_counts", "all_archive_tweets", "archive_members_observed", "communication_intensity_z")
candidate_cols <- setdiff(names(panel), structural_cols)
numeric_indicators <- candidate_cols[vapply(panel[, ..candidate_cols], is.numeric, logical(1))]

profile <- data.table::rbindlist(lapply(numeric_indicators, function(y) {
  usable <- panel[is.finite(get(y)) & is.finite(communication_intensity_z)]
  data.table::data.table(indicator = y, nonmissing_rows = nrow(usable), states = data.table::uniqueN(usable$state), months = data.table::uniqueN(usable$month), ready = nrow(usable) >= 100 && data.table::uniqueN(usable$state) >= 10 && data.table::uniqueN(usable$month) >= 6, analysis_role = ifelse(y %in% primary_indicator_names, "registered_health_indicator", "contextual_indicator"))
}), fill = TRUE)
if (!nrow(profile)) profile <- data.table::data.table(indicator = character(), nonmissing_rows = integer(), states = integer(), months = integer(), ready = logical(), analysis_role = character())
data.table::fwrite(profile, readiness_path)
if (!any(profile$ready)) {
  message("No indicator meets the descriptive association coverage rule. Wrote ", readiness_path, ".")
} else {
  indicators <- profile[ready == TRUE, indicator]
  control_vars <- grep("(^|__)control_", names(panel), value = TRUE)
  controls_rhs <- if (length(control_vars)) paste(control_vars, collapse = " + ") else "1"

  correlations <- data.table::rbindlist(lapply(indicators, function(y) {
    d <- panel[is.finite(communication_intensity_z) & is.finite(get(y)), .(x = communication_intensity_z, y = get(y))]
    pearson <- tryCatch(stats::cor.test(d$x, d$y, method = "pearson"), error = function(e) NULL)
    spearman <- tryCatch(stats::cor.test(d$x, d$y, method = "spearman", exact = FALSE), error = function(e) NULL)
    data.table::rbindlist(list(
      data.table::data.table(indicator = y, statistic_type = "pearson_correlation", estimate = if (is.null(pearson)) NA_real_ else unname(pearson$estimate), p.value = if (is.null(pearson)) NA_real_ else pearson$p.value, n = nrow(d)),
      data.table::data.table(indicator = y, statistic_type = "spearman_correlation", estimate = if (is.null(spearman)) NA_real_ else unname(spearman$estimate), p.value = if (is.null(spearman)) NA_real_ else spearman$p.value, n = nrow(d))
    ))
  }), fill = TRUE)
  correlations[, `:=`(result_status = result_status, interpretation = "descriptive bivariate association; not an effect estimate")]

  models <- lapply(indicators, function(y) {
    fml <- stats::as.formula(paste0(y, " ~ communication_intensity_z + ", controls_rhs, " | state + month"))
    fixest::feols(fml, data = panel, cluster = ~state)
  })
  names(models) <- indicators
  estimates <- data.table::rbindlist(lapply(names(models), function(y) {
    x <- data.table::as.data.table(broom::tidy(models[[y]], conf.int = TRUE))
    x[term == "communication_intensity_z", .(indicator = y, analysis_role = profile[indicator == y, analysis_role][1], association_estimate = estimate, std.error, statistic, p.value, conf.low, conf.high, n = stats::nobs(models[[y]]))]
  }), fill = TRUE)
  estimates[, `:=`(result_status = result_status, interpretation = "state- and month-adjusted conditional association; not an effect estimate")]

  prefix <- if (label_measurement_ready_for_descriptive_reporting) "descriptive" else "baseline_diagnostic_descriptive"
  data.table::fwrite(correlations, file.path(dir_outputs, paste0(prefix, "_correlations.csv")))
  data.table::fwrite(estimates, file.path(dir_outputs, paste0(prefix, "_adjusted_associations.csv")))
  saveRDS(models, file.path(dir_outputs, paste0(prefix, "_association_models.rds")))
  if (requireNamespace("modelsummary", quietly = TRUE)) modelsummary::modelsummary(models, output = file.path(dir_outputs, paste0(prefix, "_association_models.html")), title = "State- and month-adjusted descriptive associations", stars = TRUE)
  message("Wrote descriptive correlations and adjusted associations. Result status: ", result_status)
}
