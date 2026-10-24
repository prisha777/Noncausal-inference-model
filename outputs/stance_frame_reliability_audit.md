# Mandate stance and frame reliability audit

## Decision

Do not use the current A/B labels for stance or frame models. Both workbooks contain all 1,445 expected tweets and use valid category values, but the two sets of labels have no meaningful agreement beyond chance. The R pipeline therefore stopped before adjudication and did not estimate regression models.

## Results from the original independent labels

| Measure | Exact agreement | Krippendorff's alpha | Disagreements | Decision |
|---|---:|---:|---:|---|
| Mandate stance | 24.6% | -0.005 | 1,089 | Recode |
| Freedom or choice | 64.0% | 0.033 | 520 | Recode |
| Government overreach | 59.2% | -0.068 | 590 | Recode |
| Safety or effectiveness | 61.2% | -0.016 | 561 | Recode |
| Economic harm | 63.4% | 0.012 | 529 | Recode |
| Institutional distrust | 59.9% | -0.032 | 579 | Recode |
| Constitutional rights | 64.2% | 0.045 | 517 | Recode |

The coders disagreed on at least one label for 1,389 of 1,445 tweets (96.1%). An alpha near zero means that the apparent percentage agreement is what would be expected from chance given the categories the coders used. Adjudicating these files would create consensus labels, but it would not repair the reliability of the original measurement.

## Required next step

1. Do not edit these files to make the answers match. Preserve them as a failed reliability audit.
2. Use 75–100 separate pilot tweets. Two English-fluent people should code them independently without seeing party, account, the other person's labels, or the paper's expected result.
3. Compare disagreements, revise ambiguous codebook rules, and repeat the pilot until stance reaches alpha at least 0.80. Treat 0.667–0.799 as tentative only; do not use alpha below 0.667 for the paper models.
4. Freeze the revised codebook, give both coders the same main sample in different random row orders, and have them code independently.
5. Rerun `R/14_validate_stance_frame_annotations.R`. Adjudicate only after acceptable reliability is documented from the original independent labels.
6. Run `R/15_validated_stance_frame_models.R` only when the validation status is `ready_for_models`.

If the current A/B values were randomly generated, simulated, copied, or completed by one person acting as two coders, they must be discarded rather than reported as human annotation.

## Method reference

Hayes, A. F., & Krippendorff, K. (2007). Answering the call for a standard reliability measure for coding data. *Communication Methods and Measures, 1*(1), 77–89. https://doi.org/10.1080/19312450709336664
