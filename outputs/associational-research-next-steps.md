# Remaining Steps for the Noncausal Paper

The dictionary-based analysis is complete. Run it in RStudio with `source("R/13_run_paper_analysis.R")`; the paper-ready findings are written to `outputs/theory_key_findings.md`.

The only essential empirical step still requiring people is human annotation:

1. Give the two workbooks in `outputs/stance-frame-coding-20260815/` to independent coders. Each workbook contains the codebook and dropdown labels.
2. Code mandate stance as support, neutral, implicit opposition, or explicit opposition.
3. Code the six frames: freedom and choice, government overreach, safety and effectiveness, economic harm, institutional distrust, and constitutional rights.
4. Run `source("R/13_run_paper_analysis.R")` after both files are complete; R will calculate agreement and create the adjudication file.
5. Resolve the blank adjudication cells and rerun the same command. R will estimate design-weighted party-by-phase stance and frame models only for measures that pass the reliability gate.
6. If reliability is inadequate for a label, revise that code or report it as exploratory instead of forcing a result.

Do not submit the old LLM misinformation batch or use CDC, election, protest, or hate-crime measures as outcomes. The present data identify patterns in congressional message production, not exposure or causal effects on the public.
