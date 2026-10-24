# Baseline-results notice

Files whose names begin `baseline_diagnostic_` are pipeline diagnostics only. They use labels explicitly marked `keyword_baseline_not_for_final_inference`; blinded human validation is incomplete and no returned LLM batch result is present. Do not report them as substantive descriptive findings, associations, or causal estimates.

Run `source("R/10_audit_label_readiness.R")` to inspect the measurement gate. A normal `source("R/07_models.R")` run stops while labels are unvalidated. To reproduce a diagnostic strictly for pipeline debugging, set `ALLOW_BASELINE_DESCRIPTIVE_DIAGNOSTICS=1`; the script writes only files beginning `baseline_diagnostic_descriptive_`.
