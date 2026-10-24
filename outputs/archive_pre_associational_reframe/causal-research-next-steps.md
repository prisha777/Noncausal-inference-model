# From a measured association to a causal result

## What runs now

`R/07_models.R` runs on `data/processed/analysis_state_month_panel.csv`.  It
constructs a September--December 2020 exposure baseline and writes a coverage
audit.  It will not manufacture a causal coefficient when the panel lacks a
downstream outcome.

At present the panel has 1,500 state-month rows (50 states, January 2020 through
June 2022), treatment/exposure measures, and no health, election, or violence
outcomes.  Therefore the correct current result is *treatment measurement is
ready; causal outcome estimation is not yet possible*.

## Primary estimand

For a health outcome first, estimate the effect of a one-standard-deviation
increase in congressional COVID-misinformation exposure in month *t-1* on the
state outcome in month *t*, conditional on state and calendar-month fixed
effects and pre-specified time-varying confounders.

`Y_st = beta Exposure_s,t-1 + state_FE + month_FE + controls_st + error_st`

Use state-clustered uncertainty.  This is a panel association with a causal
interpretation only under conditional parallel trends/no remaining
time-varying-confounding.  The one-month lag prevents a purely simultaneous
measurement relationship; it does not by itself establish causality.

## First measurable outcome: vaccination

Use a state-month vaccination series as the primary outcome, ideally weekly CDC
administration data aggregated to month.  Define it before inspecting results:

`vaccination__completed_pct`: cumulative percent of residents fully vaccinated;
or `vaccination__new_completed_per_100k`: monthly newly completed series per
100,000 residents.

The flow measure is the preferred main outcome because a cumulative outcome
mechanically trends upward.  Merge it with the two required keys exactly:

```text
state,month,vaccination__new_completed_per_100k
CA,2021-01-01,42.1
CA,2021-02-01,88.7
```

Put the CSV in `data/raw/outcomes/`, run `R/05_prepare_outcomes.R`, then
`R/06_make_panel.R`, then `R/07_models.R`.  The model automatically reports
the coverage test in `outputs/model_readiness.csv` and writes estimates only if
there are at least 20 states, 12 months, and 240 observed rows.

## Event-study figure

Use January 2021 vaccine rollout as the event.  Define high exposure from
September--December 2020, before the event, and compare each group-month to
December 2020.  The pre-period is only four months, so it is a diagnostic, not
strong proof of parallel trends.  The model writes point estimates and 95% CIs
to `outputs/vaccine_event_study_estimates.csv` after outcomes are merged.

Do not use the March 2020 pandemic declaration as the main event study in this
panel: the data begin in January 2020 and provide only two pre-event months.

## Paper-quality credibility checks

1. Validate labels: double-code a stratified random sample of tweets; report
   precision, recall, calibration of the 0--5 score, and disagreement by party
   and time.
2. Pre-register outcome, exposure aggregation, lag, controls, and the
   September--December baseline before inspecting outcome estimates.
3. Add pre-treatment covariates measured before 2020 (political baseline,
   urbanicity, age, education) only through their interaction with time or an
   explicit matching/weighting design; state fixed effects absorb their levels.
4. Add time-varying confounders measured before the outcome (COVID incidence,
   policy stringency, vaccine supply) as `control__*` columns.  Do not control
   for a mediator such as vaccination when vaccination is the outcome.
5. Report the main lagged continuous-exposure estimate alongside a discretized
   high-exposure robustness check.  Continuous-treatment two-way fixed-effects
   coefficients can be hard to interpret under treatment-effect heterogeneity;
   do not treat a single linear coefficient as universally causal.
6. Run placebo leads (future exposure must not predict current outcome), leave-
   one-state-out estimates, alternative exposure denominators, and label-score
   thresholds.  Use randomization/permutation inference if the 50-state cluster
   count is a concern.
7. Treat violence and election outcomes as separate, pre-specified secondary
   analyses.  Election outcomes are sparse; use an election-cycle design rather
   than pretending they are monthly.

## Stronger designs if feasible

The most credible upgrade is district-month analysis with a member-to-district
crosswalk, district outcomes, and an exogenous exposure shock (for example,
plausibly exogenous platform disruption or sudden member incapacity) that
changes constituent exposure but does not directly change the outcome.  Without
such a design, frame the result as evidence consistent with a causal mechanism,
not a definitive causal claim.

For staggered binary adoption, avoid a naive two-way fixed-effect event study:
heterogeneous effects can contaminate leads and lags.  Use a group-time,
doubly-robust DiD estimator or an imputation/event-study estimator.  For this
project's continuous treatment, retain the lagged fixed-effect model as the
transparent primary association, pair it with dose bins and sensitivity checks,
and be explicit about the stronger assumptions required for a causal dose
response.
