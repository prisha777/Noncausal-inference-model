# Descriptive Analysis Design: Congressional COVID Communication on Twitter

## Scope

This project is a computational social-science and political-communication study. It maps the content, severity, timing, and distribution of COVID-related congressional messages. It does not identify the effect of messages on constituents, health behavior, elections, or political violence.

## Observable measures

The tweet archive records **elite message supply**: what congressional accounts posted. The LLM score describes the model's assessment of a tweet under a pre-specified codebook. Retweets and likes, when present, are platform engagement measures; they are not impressions or constituent exposure. State-level health and political indicators are contextual outcomes with which communication patterns may be compared.

## Descriptive units and measures

- Tweet: category, 0-5 severity, confidence, and rationale.
- Member-month: labeled-message volume, mean severity, high-severity share, and available engagement coverage.
- State-month: mean labeled-tweet severity, severity sum, high-severity share, party composition, and archive/labeled-tweet coverage.
- Party-month: distributional comparison of message volume, category, and severity.

## Analysis sequence

1. Audit corpus and label coverage.
2. Validate structured labels against the blinded stratified human-coded sample, overall and by party/period.
3. Describe temporal, partisan, chamber, geographic, and category distributions with counts, rates, confidence intervals, and figures.
4. Report Pearson and Spearman correlations between state-month communication measures and contextual health indicators.
5. Report state- and month-adjusted conditional associations as descriptive summaries. These coefficients summarize adjusted co-variation only; fixed effects do not change that interpretation.
6. Report only analyses supported by observed data. In particular, do not estimate engagement models while retweet/favorite fields are missing.

## Conceptual framework

```text
Congressional tweet -> scored message characteristic -> platform-visible engagement proxy
                         -> public information environment (unobserved)
                         -> contextual health and political indicators
```

The arrows organize theory and measurement. They are not identified causal links. The analysis can assess patterns consistent or inconsistent with the framework, not prove the pathway.

## Reporting safeguards

- Never call the state-month score "constituent exposure." Use "communication intensity" or "labeled-message severity."
- Never interpret a correlation or adjusted coefficient as an effect estimate.
- Keep the candidate-universe definition, validation design, and model-status field with every results table.
- Separate observed data from proposed future data and do not add outcome families that are not locally available.
