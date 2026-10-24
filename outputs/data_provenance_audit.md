# Data provenance and label-measurement readiness audit

Generated from the local CCIR project on 2026-07-17. This audit records the data that exist; it does not impute, infer, or substitute any missing labels.

| Item | Observed locally | Interpretation |
|---|---:|---|
| Normalized congressional tweet rows | 1,048,507 | 1,048,507 unique tweet IDs in the audit denominator |
| Time coverage | 2020-01-01 to 2022-06-30 | 30 calendar months |
| States in state-month panel | 50 | 1,500 state-month rows |
| Member screen names | 526 | As represented in normalized metadata |
| Current normalized labels | 156,509 | 14.93% of unique tweet IDs |
| Intended LLM candidate rows | 157,736 | Candidate file exists; no returned batch output file was found |
| Current label source | `keyword_baseline_not_for_final_inference` only | Not suitable for substantive descriptive reporting |
| Blinded human validation sample | 400 tweets | 50 tweets in each party × period × predicted-class stratum |
| Completed human labels | 0 | Precision, recall, and severity agreement are not yet reportable |
| COVID cases/deaths contextual-indicator panel | 1,500 rows | CDC weekly state counts aggregated to months, Jan. 2020–Jun. 2022 |
| Vaccination-flow contextual-indicator panel | 900 rows | Jan. 2021–Jun. 2022 |

## Consequence

The current numerical model files are baseline diagnostics, not paper results. The label-measurement gate requires (1) returned structured labels for the locked candidate universe and (2) completed human-coding validation, and (3) a documented review/sign-off of those metrics before a normal outcome-model run will write substantive files. The absence of labels outside the candidate universe is not automatically treated as a zero score.

## Reproducible next sequence

1. Review and lock the candidate-universe rule in `R/03_prepare_labels.R`.
2. Create the structured-label JSONL shards with `R/03c_make_llm_batch_jsonl.R`, submit them through the authorized account workflow, and place returned result JSONL files in `data/intermediate/openai_label_batches/`.
3. Parse only returned outputs with `R/03d_parse_llm_batch_results.R`.
4. Complete blinded coding in `data/intermediate/validation/human_coding_sample.csv` and run `R/09_validate_human_labels.R`.
5. Inspect `outputs/label_validation_metrics.csv` and party-stratified diagnostics; resolve any material label error or disparity before rebuilding communication measures.
6. Run `R/04_build_communication_measures.R`, `R/06_make_panel.R`, and then `R/07_models.R`.

The archived case/death series may include retrospective reporting revisions; the pipeline retains reported values and uses `log(1 + monthly_count)` for the registered health-indicator transform. Negative revision values should be separately reviewed before final reporting.
