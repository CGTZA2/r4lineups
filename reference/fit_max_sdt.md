# MAX SDT Compound-Decision Model for Eyewitness Lineups

Fits the MAX signal-detection (compound-decision) model to eyewitness
lineup data using chi-squared goodness-of-fit minimization. Estimates d'
(sensitivity) and lambda (criterion) simultaneously from hit rates,
false-alarm rates, and rejection rates, supporting one or two conditions
and optional model constraints.

## Usage

``` r
fit_max_sdt(
  n_hit,
  n_tp_choose,
  n_fa,
  N_tp,
  N_ta,
  n_hit_2 = NULL,
  n_tp_choose_2 = NULL,
  n_fa_2 = NULL,
  N_tp_2 = NULL,
  N_ta_2 = NULL,
  n = 6,
  constrain_d = FALSE,
  constrain_c = FALSE,
  start = NULL,
  nboot = 0,
  ci_level = 0.95,
  seed = NULL
)
```

## Arguments

- n_hit:

  Number of correct suspect identifications in target-present (TP)
  lineups (condition 1).

- n_tp_choose:

  Total number of participants who chose any lineup member in TP lineups
  (condition 1). Must be \>= n_hit.

- n_fa:

  Number of participants who chose any lineup member in target-absent
  (TA) lineups (condition 1).

- N_tp:

  Total number of participants in TP lineups (condition 1).

- N_ta:

  Total number of participants in TA lineups (condition 1).

- n_hit_2, n_tp_choose_2, n_fa_2, N_tp_2, N_ta_2:

  Corresponding counts for an optional second condition. All five must
  be supplied together or not at all.

- n:

  Lineup size (number of members). Default 6.

- constrain_d:

  Logical. If `TRUE` and two conditions are supplied, constrains d' to
  be equal across conditions. Default `FALSE`.

- constrain_c:

  Logical. If `TRUE` and two conditions are supplied, constrains lambda
  to be equal across conditions. Default `FALSE`.

- start:

  Named numeric vector of starting values for the optimizer. If `NULL`
  (default), reasonable starting values are chosen automatically. Names
  should be `"d1"`, `"lambda1"` (and `"d2"`, `"lambda2"` for two
  conditions).

- nboot:

  Number of parametric bootstrap replicates for confidence intervals.
  Default 0 (no bootstrap). Set to e.g. 1000 to obtain bootstrap CIs.

- ci_level:

  Confidence level for bootstrap CIs. Default 0.95.

- seed:

  Optional integer seed for reproducible bootstrap.

## Value

An object of class `"max_sdt_fit"` containing:

- dprime_1, lambda_1:

  Estimated d' and criterion for condition 1.

- dprime_2, lambda_2:

  Estimated d' and criterion for condition 2 (if supplied).

- chisq:

  Chi-squared goodness-of-fit statistic.

- df:

  Degrees of freedom for the GoF test.

- p_value:

  P-value for the GoF test.

- observed:

  Named vector of observed counts.

- predicted:

  Named vector of predicted counts.

- convergence:

  Convergence code from [`optim()`](https://rdrr.io/r/stats/optim.html)
  (0 = success).

- constrain_d, constrain_c, n_conditions, n:

  Input metadata.

- boot_ci:

  Bootstrap confidence intervals (if nboot \> 0).

- boot_params:

  Full bootstrap parameter matrix (if nboot \> 0).

## Details

The MAX SDT compound-decision model (Smith, 2022; Gourevitch & Galanter,
1967) addresses the fundamental challenge of eyewitness lineup research:
participants make only one identification response, so within-person ROC
analysis is impossible. Group-level d' inference requires a model that
jointly accounts for detection (is the target present?) and
identification (which member is the target?).

For a lineup of \\n\\ members with culprit signal strength \\d'\\ and
response criterion \\\lambda\\: \$\$P(\text{correct ID}) =
\int\_{\lambda}^{\infty} \phi(x - d') \Phi(x)^{n-1}\\dx\$\$
\$\$P(\text{choose anyone} \mid \text{TP}) = 1 - \Phi(\lambda -
d')\Phi(\lambda)^{n-1}\$\$ \$\$P(\text{choose anyone} \mid \text{TA}) =
1 - \Phi(\lambda)^n\$\$

Parameters are estimated by minimizing the chi-squared discrepancy
between observed and predicted frequencies. Nested model comparisons
(equal d' or equal lambda across conditions) are supported via
[`compare_max_sdt`](https://cgtza2.github.io/r4lineups/reference/compare_max_sdt.md).

Parametric bootstrap CIs are obtained by simulating data from the fitted
model and re-fitting.

## References

Gourevitch, V., & Galanter, E. (1967). A significance test for
one-parameter isosensitivity functions. *Psychometrika, 32*(1), 25-33.

Smith, A. M. (2022). Culprit and victim lineups. Unpublished analysis
scripts.

## See also

[`compare_max_sdt`](https://cgtza2.github.io/r4lineups/reference/compare_max_sdt.md),
[`estimate_msdt_params`](https://cgtza2.github.io/r4lineups/reference/estimate_msdt_params.md),
[`sdt_compare`](https://cgtza2.github.io/r4lineups/reference/sdt_compare.md)

## Examples

``` r
# Single condition
fit1 <- fit_max_sdt(
  n_hit = 69, n_tp_choose = 82, n_fa = 64,
  N_tp = 96, N_ta = 106, n = 6
)
print(fit1)
#> MAX SDT Compound-Decision Model
#>   Lineup size: 6 | Conditions: 1 | Free parameters: 2
#>   Convergence: OK
#> 
#> Parameter estimates:
#>   d' = 1.944 | lambda = 1.146
#> 
#> Goodness of fit: chi-sq(1) = 4.408, p = 0.0358
#> 
#> Observed vs predicted:
#>           n_hit n_filler_tp n_reject_tp n_fa n_reject_ta
#> Observed   69.0        13.0        14.0 64.0        42.0
#> Predicted  66.4        19.2        10.4 58.7        47.3

# Two conditions, free parameters
fit_free <- fit_max_sdt(
  n_hit = 69, n_tp_choose = 82, n_fa = 64,
  N_tp = 96, N_ta = 106,
  n_hit_2 = 67, n_tp_choose_2 = 78, n_fa_2 = 42,
  N_tp_2 = 90, N_ta_2 = 96, n = 6
)

# Two conditions, equal d' (constrained)
fit_eqd <- fit_max_sdt(
  n_hit = 69, n_tp_choose = 82, n_fa = 64,
  N_tp = 96, N_ta = 106,
  n_hit_2 = 67, n_tp_choose_2 = 78, n_fa_2 = 42,
  N_tp_2 = 90, N_ta_2 = 96, n = 6,
  constrain_d = TRUE
)

# Compare models
cmp <- compare_max_sdt(fit_free, fit_eqd)
print(cmp)
#> MAX SDT Model Comparison (chi-squared difference test)
#>   Free model:       chi-sq = 4.659 (4 free params)
#>   Constrained model: chi-sq = 6.081 (3 free params)
#>   Constraint: d' equal
#>   Delta chi-sq(1) = 1.422, p = 0.2331
#>   Interpretation: constraint does not significantly worsen fit.
```
