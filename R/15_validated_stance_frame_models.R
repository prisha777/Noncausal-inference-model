# Party-by-phase stance and frame models. Runs only after two-coder validation.
source("R/00_config.R")

required <- c("data.table", "fixest", "broom", "ggplot2", "scales")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing packages: ", paste(missing, collapse = ", "))

validation_dir <- file.path(dir_intermediate, "validation")
validated_path <- file.path(validation_dir, "mandate_stance_frame_validated_labels.csv")
agreement_path <- file.path(dir_outputs, "stance_frame_intercoder_agreement.csv")
validation_readiness_path <- file.path(dir_outputs, "stance_frame_validation_readiness.csv")
model_readiness_path <- file.path(dir_outputs, "stance_frame_model_readiness.csv")

phase_levels <- c(
  "Emergency onset", "Pre-vaccine contestation", "Vaccine rollout",
  "Delta and mandates", "Omicron period"
)
stance_levels <- c("support", "neutral", "implicit_opposition", "explicit_opposition")
frame_columns <- c(
  "frame_freedom_choice", "frame_government_overreach", "frame_safety_effectiveness",
  "frame_economic_harm", "frame_institutional_distrust", "frame_constitutional_rights"
)

write_model_readiness <- function(status, detail) {
  data.table::fwrite(
    data.table::data.table(status = status, detail = detail),
    model_readiness_path
  )
}

party_phase_contrasts <- function(model, outcome) {
  coefficients <- stats::coef(model)
  covariance <- stats::vcov(model)
  data.table::rbindlist(lapply(phase_levels, function(phase_name) {
    terms <- "republican"
    if (phase_name != phase_levels[1]) {
      interaction_term <- paste0("republican:phase", phase_name)
      if (interaction_term %in% names(coefficients)) terms <- c(terms, interaction_term)
    }
    if (!all(terms %in% names(coefficients))) {
      return(data.table::data.table(
        outcome = outcome, phase = phase_name, odds_ratio = NA_real_,
        conf.low = NA_real_, conf.high = NA_real_, std.error = NA_real_
      ))
    }
    estimate <- sum(coefficients[terms])
    variance <- sum(covariance[terms, terms, drop = FALSE])
    standard_error <- sqrt(variance)
    data.table::data.table(
      outcome = outcome,
      phase = phase_name,
      odds_ratio = exp(estimate),
      conf.low = exp(estimate - 1.96 * standard_error),
      conf.high = exp(estimate + 1.96 * standard_error),
      std.error = standard_error
    )
  }), fill = TRUE)
}

run_validated_stance_frame_models <- function() {
  if (!file.exists(validation_readiness_path)) {
    write_model_readiness("awaiting_validated_human_labels", "Complete both coder sheets and adjudication, then rerun R/14_validate_stance_frame_annotations.R.")
    message("Stance/frame models are gated: validated human labels do not yet exist.")
    return(invisible(FALSE))
  }
  validation_record <- data.table::fread(validation_readiness_path)
  validation_status <- validation_record$status[1]
  if (!identical(validation_status, "ready_for_models")) {
    write_model_readiness(
      validation_status,
      paste("Models were not estimated.", validation_record$detail[1])
    )
    message("Stance/frame models remain blocked. Validation status: ", validation_status)
    return(invisible(FALSE))
  }
  if (!file.exists(validated_path) || !file.exists(agreement_path)) {
    write_model_readiness("awaiting_validated_human_labels", "Validation says ready, but the validated labels or agreement file is missing. Rerun R/14_validate_stance_frame_annotations.R.")
    return(invisible(FALSE))
  }

  d <- data.table::fread(validated_path)
  agreement <- data.table::fread(agreement_path)
  d[, `:=`(
    month = data.table::as.IDate(format(as.POSIXct(tweet_date, tz = "UTC"), "%Y-%m-01")),
    phase = factor(phase, levels = phase_levels),
    republican = as.integer(party == "Republican"),
    senate = as.integer(chamber == "Senate")
  )]
  if (anyNA(d[, .(month, phase, republican, senate, state, screen_name, sample_weight)])) {
    stop("Validated data contain missing model metadata. Inspect the coding key before modeling.")
  }

  stance_gate <- agreement[measure == "stance_label", reliability_gate_0_667][1]
  if (!isTRUE(stance_gate)) {
    write_model_readiness("stance_reliability_gate_failed", "Revise stance coding rules and recode before estimating stance models.")
    message("Stance reliability did not pass the 0.667 gate; no human-coded models were estimated.")
    return(invisible(FALSE))
  }

  weighted_share <- function(outcome, subset_data = d) {
    subset_data[, .(
      weighted_share = stats::weighted.mean(get(outcome), sample_weight),
      unweighted_n = .N,
      accounts = data.table::uniqueN(screen_name),
      weighted_population = sum(sample_weight)
    ), by = .(party, phase)][, outcome := outcome][]
  }

  model_one_binary <- function(outcome, model_data) {
    if (data.table::uniqueN(model_data[[outcome]]) < 2L) return(NULL)
    tryCatch({
      fit <- fixest::feglm(
        stats::as.formula(paste0(outcome, " ~ republican + senate + republican:phase | state + month")),
        family = "binomial", weights = ~sample_weight, cluster = ~screen_name,
        data = model_data
      )
      if (!isTRUE(fit$convStatus)) {
        message("Skipped ", outcome, ": model did not converge.")
        return(NULL)
      }
      fit
    },
      error = function(e) {
        message("Skipped ", outcome, ": ", conditionMessage(e))
        NULL
      }
    )
  }

  for (level in stance_levels) d[, (paste0("stance_", level)) := as.integer(stance_label == level)]
  stance_outcomes <- paste0("stance_", stance_levels)
  stance_models <- setNames(lapply(stance_outcomes, model_one_binary, model_data = d), stance_outcomes)
  stance_models <- stance_models[!vapply(stance_models, is.null, logical(1))]
  if (!length(stance_models)) stop("No stance model had outcome variation.")

  stance_shares <- data.table::rbindlist(lapply(stance_outcomes, weighted_share), fill = TRUE)
  data.table::fwrite(stance_shares, file.path(dir_outputs, "validated_stance_party_phase_shares.csv"))
  stance_coefficients <- data.table::rbindlist(lapply(names(stance_models), function(outcome) {
    x <- data.table::as.data.table(broom::tidy(stance_models[[outcome]], conf.int = TRUE))
    x[, outcome := outcome]
    x
  }), fill = TRUE)
  data.table::fwrite(stance_coefficients, file.path(dir_outputs, "validated_stance_model_coefficients.csv"))
  stance_contrasts <- data.table::rbindlist(lapply(names(stance_models), function(outcome) {
    party_phase_contrasts(stance_models[[outcome]], outcome)
  }), fill = TRUE)
  data.table::fwrite(stance_contrasts, file.path(dir_outputs, "validated_stance_party_contrasts_by_phase.csv"))

  opposition <- d[stance_label %in% c("implicit_opposition", "explicit_opposition")]
  eligible_frames <- agreement[
    measure %in% frame_columns & reliability_gate_0_667 == TRUE,
    measure
  ]
  frame_models <- list()
  if (nrow(opposition) && length(eligible_frames)) {
    frame_models <- setNames(lapply(eligible_frames, model_one_binary, model_data = opposition), eligible_frames)
    frame_models <- frame_models[!vapply(frame_models, is.null, logical(1))]
  }
  frame_shares <- if (nrow(opposition) && length(eligible_frames)) {
    data.table::rbindlist(lapply(eligible_frames, weighted_share, subset_data = opposition), fill = TRUE)
  } else data.table::data.table()
  data.table::fwrite(frame_shares, file.path(dir_outputs, "validated_frame_party_phase_shares.csv"))

  if (length(frame_models)) {
    frame_coefficients <- data.table::rbindlist(lapply(names(frame_models), function(outcome) {
      x <- data.table::as.data.table(broom::tidy(frame_models[[outcome]], conf.int = TRUE))
      x[, outcome := outcome]
      x
    }), fill = TRUE)
    frame_contrasts <- data.table::rbindlist(lapply(names(frame_models), function(outcome) {
      party_phase_contrasts(frame_models[[outcome]], outcome)
    }), fill = TRUE)
  } else {
    frame_coefficients <- data.table::data.table()
    frame_contrasts <- data.table::data.table()
  }
  data.table::fwrite(frame_coefficients, file.path(dir_outputs, "validated_frame_model_coefficients.csv"))
  data.table::fwrite(frame_contrasts, file.path(dir_outputs, "validated_frame_party_contrasts_by_phase.csv"))
  saveRDS(list(stance = stance_models, frames = frame_models), file.path(dir_outputs, "validated_stance_frame_models.rds"))

  stance_plot <- ggplot2::ggplot(
    stance_shares,
    ggplot2::aes(x = phase, y = 100 * weighted_share, color = party, group = party)
  ) +
    ggplot2::geom_line(linewidth = 0.75) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::facet_wrap(~outcome, scales = "free_y", ncol = 2,
                        labeller = ggplot2::labeller(outcome = function(x) gsub("stance_|_", " ", x))) +
    ggplot2::scale_color_manual(values = c(Democratic = "#2E6F9E", Republican = "#B64949")) +
    ggplot2::labs(
      x = NULL, y = "Design-weighted share (%)", color = NULL,
      title = "Human-coded mandate stance by party and pandemic phase",
      subtitle = "Two-coder validated labels; descriptive estimates, not causal effects"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1), legend.position = "top")
  ggplot2::ggsave(file.path(dir_outputs, "validated_stance_by_party_phase.png"), stance_plot,
                  width = 9, height = 7, dpi = 300)

  if (nrow(frame_shares)) {
    frame_plot <- ggplot2::ggplot(
      frame_shares,
      ggplot2::aes(x = phase, y = 100 * weighted_share, color = party, group = party)
    ) +
      ggplot2::geom_line(linewidth = 0.75) +
      ggplot2::geom_point(size = 1.8) +
      ggplot2::facet_wrap(~outcome, scales = "free_y", ncol = 2,
                          labeller = ggplot2::labeller(outcome = function(x) gsub("frame_|_", " ", x))) +
      ggplot2::scale_color_manual(values = c(Democratic = "#2E6F9E", Republican = "#B64949")) +
      ggplot2::labs(
        x = NULL, y = "Design-weighted share among opposition tweets (%)", color = NULL,
        title = "Human-coded rhetorical frames by party and pandemic phase",
        subtitle = "Restricted to implicit or explicit opposition; descriptive, not causal"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1), legend.position = "top")
    ggplot2::ggsave(file.path(dir_outputs, "validated_frames_by_party_phase.png"), frame_plot,
                    width = 9, height = 8, dpi = 300)
  }

  diagnostics <- data.table::data.table(
    model_family = c(rep("stance one-versus-rest", length(stance_models)), rep("frame among opposition", length(frame_models))),
    outcome = c(names(stance_models), names(frame_models)),
    observations = c(vapply(stance_models, stats::nobs, numeric(1)), vapply(frame_models, stats::nobs, numeric(1))),
    accounts = c(
      rep(data.table::uniqueN(d$screen_name), length(stance_models)),
      rep(data.table::uniqueN(opposition$screen_name), length(frame_models))
    ),
    weighting = "inverse-probability sampling weights",
    fixed_effects = "state + month",
    uncertainty = "standard errors clustered by account",
    interpretation = "descriptive conditional association; not causal"
  )
  data.table::fwrite(diagnostics, file.path(dir_outputs, "validated_stance_frame_model_diagnostics.csv"))
  write_model_readiness(
    "complete",
    paste(length(stance_models), "stance models and", length(frame_models), "frame models estimated; low-reliability frames were excluded.")
  )
  message("Validated stance/frame results are complete. Read the validated_* files in outputs/.")
  invisible(TRUE)
}

validated_stance_frame_models_ready <- run_validated_stance_frame_models()
