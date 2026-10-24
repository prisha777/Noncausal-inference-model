# One-click RStudio runner for the noncausal paper.
#
# In RStudio: open the health_polarization project/folder, open this file, and click
# Source. The script stops on missing inputs and verifies every required output.

source("R/11_theory_associational_models.R")
source("R/12_prepare_stance_annotation.R")
source("R/14_validate_stance_frame_annotations.R")
source("R/15_validated_stance_frame_models.R")

required_outputs <- c(
  "theory_corpus_summary.csv",
  "theory_party_phase_descriptives.csv",
  "theory_party_month_descriptives.csv",
  "theory_monthly_party_contrasts.csv",
  "theory_alternative_phase_descriptives.csv",
  "theory_model_results.csv",
  "theory_party_contrasts_by_phase.csv",
  "theory_mandate_predicted_probabilities.csv",
  "theory_model_diagnostics.csv",
  "theory_concentration_summary.csv",
  "theory_mandate_concentration_summary.csv",
  "theory_concentration_benchmarks.csv",
  "theory_lorenz_coordinates.csv",
  "theory_party_phase_without_top10_mandate_accounts.csv",
  "theory_key_findings.md",
  "theory_covid_posting_by_party_phase.png",
  "theory_mandate_opposition_by_party_phase.png",
  "theory_monthly_covid_posting_by_party.png",
  "theory_monthly_mandate_opposition_by_party.png",
  "theory_monthly_trajectories_by_party.png",
  "theory_concentration_lorenz.png",
  "mandate_stance_annotation_sample_summary.csv",
  "stance_frame_validation_readiness.csv",
  "stance_frame_model_readiness.csv"
)
paths <- file.path(dir_outputs, required_outputs)
missing_outputs <- paths[!file.exists(paths)]
empty_outputs <- paths[file.exists(paths) & file.info(paths)$size == 0]

if (length(missing_outputs)) {
  stop("Analysis finished but required outputs are missing: ", paste(missing_outputs, collapse = ", "))
}
if (length(empty_outputs)) {
  stop("Analysis finished but required outputs are empty: ", paste(empty_outputs, collapse = ", "))
}

message("Paper analysis complete. Start with outputs/theory_key_findings.md")
message("Figures and CSV tables are in outputs/.")
message("If human coding is incomplete, read outputs/stance_frame_validation_readiness.csv for the exact next action.")
