# Winter 2-HT Model

## Introduction

This vignette demonstrates the **two-high-threshold (2-HT) model** for
lineup data described by Winter et al. The model estimates four
parameters from full 2 × 3 outcomes (target-present/target-absent by
suspect ID/filler ID/rejection):

- **dP**: detection probability for target-present lineups
- **dA**: detection probability for target-absent lineups
- **b**: biased suspect selection
- **g**: guessing rate

The implementation in **r4lineups** accepts either raw data (data frame)
or aggregated counts.

## Data format

We use the built-in `lineup_example` dataset, which already contains the
required columns:

``` r

library(r4lineups)
data(lineup_example)
head(lineup_example)
#>   target_present identification confidence
#> 1           TRUE        suspect         90
#> 2           TRUE         reject         50
#> 3           TRUE        suspect         90
#> 4           TRUE         filler         50
#> 5           TRUE         filler         50
#> 6           TRUE        suspect         90
```

## 1. Fit the model using counts

First, convert the data to the 2 × 3 count format expected by
[`fit_winter_2ht()`](https://cgtza2.github.io/r4lineups/reference/fit_winter_2ht.md):

``` r

counts <- c(
  n_tp_suspect = sum(lineup_example$target_present & lineup_example$identification == "suspect"),
  n_tp_filler = sum(lineup_example$target_present & lineup_example$identification == "filler"),
  n_tp_reject = sum(lineup_example$target_present & lineup_example$identification == "reject"),
  n_ta_suspect = sum(!lineup_example$target_present & lineup_example$identification == "suspect"),
  n_ta_filler = sum(!lineup_example$target_present & lineup_example$identification == "filler"),
  n_ta_reject = sum(!lineup_example$target_present & lineup_example$identification == "reject")
)

fit <- fit_winter_2ht(counts, lineup_size = 6)
fit$parameters
#>        dP        dA         b         g 
#> 0.4874993 0.3166677 0.1492703 0.4954122
```

## 2. Model diagnostics and plots

``` r

# Parameter plot (with CI if bootstrapped)
plot_2ht_parameters(fit)

# Observed vs. predicted response proportions
plot_2ht_fit(fit)
```

## 3. Bootstrap confidence intervals

``` r

set.seed(123)
boot_fit <- boot_winter_2ht(
  counts,
  lineup_size = 6,
  nboot = 200,
  conf_level = 0.95,
  method = "percentile"
)

boot_fit$ci
#>         Lower     Upper
#> dP 0.32848712 0.6130046
#> dA 0.00000000 0.5620768
#> b  0.05489949 0.2598609
#> g  0.35640748 0.6535679
```

## 4. Notes and interpretation

- **dP** and **dA** index detection rates for guilty and innocent
  lineups, respectively.
- **b** reflects biased suspect selection beyond guessing.
- **g** indexes guessing when no detection is made.

For larger datasets, increase `nboot` to 1000 or more for stable
confidence intervals.
