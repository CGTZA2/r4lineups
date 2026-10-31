# Winter et al. (2022) Two-High Threshold (2-HT) MPT Model for Eyewitness Identification

Fits the 2-HT multinomial processing tree (MPT) model to lineup
identification data. This model separates detection-based processes (dP,
dA) from non-detection-based processes (biased selection b,
guessing-based selection g) using the full 2x3 outcome structure of
lineup procedures.

## Usage

``` r
fit_winter_2ht(
  data,
  lineup_size = 6,
  target_present = NULL,
  identification = NULL,
  start_params = NULL,
  method = "L-BFGS-B",
  ...
)
```

## Arguments

- data:

  A data frame with lineup identification data, OR a named vector/list
  of counts with the following elements: - n_tp_suspect: Number of
  suspect IDs in target-present (TP) lineups - n_tp_filler: Number of
  filler IDs in TP lineups - n_tp_reject: Number of rejections in TP
  lineups - n_ta_suspect: Number of suspect IDs in target-absent (TA)
  lineups - n_ta_filler: Number of filler IDs in TA lineups -
  n_ta_reject: Number of rejections in TA lineups

- lineup_size:

  Size of the lineup (L). Default is 6.

- target_present:

  Column name indicating target presence (TRUE/FALSE) if data is a data
  frame.

- identification:

  Column name for identification decision if data is a data frame.
  Should contain "suspect", "filler", or "reject".

- start_params:

  Optional named vector of starting parameter values (dP, dA, b, g). If
  NULL, uses reasonable defaults.

- method:

  Optimization method. Default is "L-BFGS-B" for bounded optimization.

- ...:

  Additional arguments passed to optim().

## Value

An object of class "winter_2ht" containing:

- parameters:

  Estimated parameters (dP, dA, b, g)

- se:

  Standard errors of parameter estimates

- loglik:

  Log-likelihood of the fitted model

- aic:

  Akaike Information Criterion

- bic:

  Bayesian Information Criterion

- fitted_probs:

  Model-predicted probabilities for each outcome

- observed_counts:

  Observed counts

- expected_counts:

  Expected counts under the model

- lineup_size:

  Lineup size used

- convergence:

  Convergence code from optim

- n_total:

  Total sample size

## Details

The 2-HT model (Winter, Menne, Bell, & Buchner, 2022) uses four
parameters:

- **dP**: Probability of detecting culprit presence (0 to 1)

- **dA**: Probability of detecting culprit absence (0 to 1)

- **b**: Probability of biased suspect selection (0 to 1)

- **g**: Probability of guessing-based selection (0 to 1)

Model equations for culprit-present lineups:

- P(suspect ID) = dP + (1-dP) \* \[b + (1-b) \* g \* (1/L)\]

- P(filler ID) = (1-dP) \* (1-b) \* g \* ((L-1)/L)

- P(reject) = (1-dP) \* (1-b) \* (1-g)

Model equations for culprit-absent lineups:

- P(suspect ID) = (1-dA) \* \[b + (1-b) \* g \* (1/L)\]

- P(filler ID) = (1-dA) \* (1-b) \* g \* ((L-1)/L)

- P(reject) = dA + (1-dA) \* (1-b) \* (1-g)

With one aggregate 2 by 3 outcome table, the four free parameters fit
the four independent cell proportions, so the model is saturated and has
no residual goodness-of-fit degrees of freedom. Inference about model
fit or restrictions requires additional conditions, parameter
constraints, or participant/item-level replication; the reported AIC/BIC
describe this likelihood fit but do not by themselves validate the
process interpretation.

A feasible inverse of the published tree supplies the saturated interior
solution; otherwise bounded optimization uses multiple starts. Regular
Hessian SEs are returned only for interior fits. Boundary fits retain
their estimates but have unavailable regular SEs. A low rejection
percentage alone is not used as a criterion for refusing a fit.

## References

Winter, K., Menne, N. M., Bell, R., & Buchner, A. (2022). Experimental
validation of a multinomial processing tree model for analyzing
eyewitness identification decisions. Scientific Reports, 12, 15571.
https://doi.org/10.1038/s41598-022-19513-w

## Examples

``` r
# Example 1: Using count data directly
counts <- c(
  n_tp_suspect = 147, n_tp_filler = 94, n_tp_reject = 141,
  n_ta_suspect = 38, n_ta_filler = 138, n_ta_reject = 206
)
fit <- fit_winter_2ht(counts, lineup_size = 6)
#> Warning: 2-HT estimate is on a parameter boundary; regular Hessian standard errors are unavailable. Bootstrap intervals also need boundary qualification.
print(fit)
#> 
#> Winter et al. (2022) Two-High Threshold MPT Model
#> ==================================================
#> 
#> Sample size:
#>   Target-present: 382
#>   Target-absent:  382
#>   Total:          764
#>   Lineup size:    6
#> 
#> Parameter Estimates:
#>                       Estimate SE
#> dP (culprit presence)   0.3169 NA
#> dA (culprit absence)    0.0000 NA
#> b  (biased selection)   0.0273 NA
#> g  (guessing)           0.4452 NA
#> 
#> Log-likelihood: -768.13
#> AIC: 1544.26
#> BIC: 1562.82
summary(fit)
#> 
#> Winter et al. (2022) Two-High Threshold MPT Model
#> ==================================================
#> 
#> Sample size:
#>   Target-present: 382
#>   Target-absent:  382
#>   Total:          764
#>   Lineup size:    6
#> 
#> Parameter Estimates:
#>                       Estimate SE Lower_95 Upper_95
#> dP (culprit presence)   0.3169 NA       NA       NA
#> dA (culprit absence)    0.0000 NA       NA       NA
#> b  (biased selection)   0.0273 NA       NA       NA
#> g  (guessing)           0.4452 NA       NA       NA
#> 
#> 
#> Model Fit:
#>   Log-likelihood: -768.13
#>   AIC: 1544.26
#>   BIC: 1562.82
#> 
#> Observed vs. Expected Counts:
#> 
#> Target-Present Lineups:
#>            Observed Expected      Residual
#> Suspect ID      147    147.0 -1.223799e-06
#> Filler ID        94     94.2 -1.623483e-01
#> Reject          141    140.8  1.623496e-01
#> 
#> Target-Absent Lineups:
#>            Observed Expected      Residual
#> Suspect ID       38     38.0 -1.963184e-08
#> Filler ID       138    137.8  1.623490e-01
#> Reject          206    206.2 -1.623489e-01
#> 
#> Model identification:
#>   Saturated (just-identified): 4 parameters, 4 data df, 0 residual df.
#>   Absolute fit is not testable from a single condition (the model reproduces the
#>   data by construction); use multiple conditions or constraints to test fit.
#>   Residual obs-vs-expected discrepancy = 0.0007861 (~= 0 confirms convergence).

# Example 2: Using a data frame
data(lineup_example)
fit <- fit_winter_2ht(
  lineup_example,
  lineup_size = 6,
  target_present = "target_present",
  identification = "identification"
)
```
