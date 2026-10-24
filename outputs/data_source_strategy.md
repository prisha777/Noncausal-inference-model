# Data Source Strategy

## Congressional communication corpus

1. **Miller & Jiang / Zenodo 117th Congress dataset** supplies a structured backbone for the 117th Congress.
2. **alexlitel/congresstweets** supplies complementary longitudinal coverage and source cross-checks.
3. **Kaggle US Congress Members Tweets** may be used only for supplementary coverage after account, time, and duplication checks.
4. **Congressional metadata** link account handles to party, state, chamber, and member identifiers.

The active local normalized corpus spans January 2020 through June 2022. Every source is retained in the normalized file so that coverage and duplication can be audited.

## Contextual indicators

The local outcomes folder currently contains CDC state-month cases, deaths, and vaccination-flow data. These files are used to describe contextual correspondence with message patterns. They are not treated as downstream outcomes of congressional communication.

Election, protest, and hate-crime files may be added later only after their coverage, temporal unit, and measurement limitations are documented. They should be analysed as separate contextual domains, not pooled into a single outcome model.

## Design contribution

The project contributes a reproducible system for: (a) collecting and auditing elite COVID communication, (b) coding message categories and severity using a validated structured rubric, and (c) mapping how those message patterns vary by time, party, chamber, member, and state. Its central object is elite message supply, not a quasi-experimental design or an effects claim.
