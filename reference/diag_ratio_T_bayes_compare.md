# Bayesian Comparison of Two Diagnosticity Ratios

Fits independent Beta-Binomial models to two lineup pairs and returns
the posterior distribution of the difference \\\Delta = DR_A - DR_B\\,
together with \\P(DR_A \> DR_B \mid \mathbf{n})\\.

## Usage

``` r
diag_ratio_T_bayes_compare(
  pres_A,
  abs_A,
  pos_pres_A,
  pos_abs_A,
  k1_A,
  k2_A,
  pres_B,
  abs_B,
  pos_pres_B,
  pos_abs_B,
  k1_B,
  k2_B,
  alpha = 0.5,
  S = 10000,
  credible_mass = 0.95
)
```

## Arguments

- pres_A, abs_A:

  Numeric vectors of lineup choices for lineup pair A (target-present
  and target-absent, respectively).

- pos_pres_A, pos_abs_A:

  Suspect positions for pair A.

- k1_A, k2_A:

  Lineup sizes for pair A (for validation).

- pres_B, abs_B:

  Numeric vectors of lineup choices for lineup pair B.

- pos_pres_B, pos_abs_B:

  Suspect positions for pair B.

- k1_B, k2_B:

  Lineup sizes for pair B.

- alpha:

  Prior concentration (shared). Default 0.5 (Jeffreys).

- S:

  Number of posterior draws. Default 10000.

- credible_mass:

  Width of credible interval. Default 0.95.

## Value

An object of class `"diag_ratio_T_bayes_compare"`.

## See also

[`diag_ratio_T_bayes`](https://cgtza2.github.io/r4lineups/reference/diag_ratio_T_bayes.md)

## Examples

``` r
set.seed(1)
pres_A <- round(runif(100, 1, 6)); abs_A <- round(runif(80, 1, 6))
pres_B <- round(runif(100, 1, 6)); abs_B <- round(runif(80, 1, 6))
res <- diag_ratio_T_bayes_compare(pres_A, abs_A, 3, 3, 6, 6,
                                   pres_B, abs_B, 3, 3, 6, 6)
print(res)
#> Bayesian comparison of diagnosticity ratios (DR_A - DR_B)
#>   DR_A posterior mean: 1.365
#>   DR_B posterior mean: 0.918
#>   Delta posterior mean:   0.447
#>   Delta posterior median: 0.421
#>   95% CI on delta: [-0.370, 1.445]
#>   P(DR_A > DR_B | data): 0.8532
#>   P(DR_B > DR_A | data): 0.1468
```
