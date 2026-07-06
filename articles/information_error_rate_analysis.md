# Information and Error-Rate Metrics

## Introduction

This vignette demonstrates two complementary approaches for evaluating
lineup evidence:

1.  **Expected Information Gain (EIG)** following Starns et al., which
    quantifies how much a response reduces uncertainty about guilt.
2.  **PPV-range estimation** following Fitzgerald, Tredoux, and Juncu,
    which reports how positive predictive value changes under different
    corrections for lineup size.

Both methods use the same data format as ROC/CAC analyses: a data frame
with `target_present`, `identification`, and `confidence` columns.

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

## 1. Expected Information Gain (EIG)

EIG summarizes how informative lineup responses are about guilt. Higher
values indicate greater uncertainty reduction. You can compute EIG with
or without confidence binning.

``` r

# Compute EIG with binned confidence
 eig_obj <- make_eig(
  lineup_example,
  confidence_bins = c(0, 60, 80, 100),
  show_plot = FALSE
)

# EIG value (bits)
eig_obj$eig
#> [1] 0.2836494
```

### Response-level information gain

The EIG object includes per-response information gain and posterior
probabilities.

``` r

head(eig_obj$response_data)
#> # A tibble: 6 × 11
#>   response         n_guilty n_innocent p_x_given_guilty p_x_given_innocent
#>   <chr>               <dbl>      <dbl>            <dbl>              <dbl>
#> 1 suspect_(80,100]       38          0             0.38               0   
#> 2 filler_(60,80]          1          7             0.01               0.07
#> 3 reject_[0,60]          22         61             0.22               0.61
#> 4 suspect_(60,80]        17         10             0.17               0.1 
#> 5 filler_[0,60]          17         17             0.17               0.17
#> 6 suspect_[0,60]          5          5             0.05               0.05
#> # ℹ 6 more variables: identification <chr>, confidence_level <chr>,
#> #   p_response <dbl>, posterior_guilty <dbl>, posterior_entropy <dbl>,
#> #   information_gain <dbl>
```

### Plots

``` r

# Information gain by response category
plot_eig(eig_obj)

# Posterior probability of guilt by response category
plot_eig_posteriors(eig_obj)
```

## 2. PPV-range estimation

PPV-range estimation compares **nominal-size**, **effective-size**, and
**no correction** assumptions about false IDs. This yields a range of
plausible PPV values rather than a single estimate.

``` r

ppv_range <- make_ppv_range(
  lineup_example,
  lineup_size = 6,
  show_plots = FALSE
)

# Overall PPV under each correction
ppv_range$ppv_nominal$overall_ppv
#> [1] 0.7719298
ppv_range$ppv_effective$overall_ppv
#> [1] 0.6233097
ppv_range$ppv_none$overall_ppv
#> [1] 0.3606557
```

### Plots

``` r

# PPV range across confidence bins
plot_ppv_range(ppv_range)

# Effective size (confidence-based) used in corrections
plot_effective_size_conf(ppv_range)
```

## Interpretation guidance

- **EIG** is best for comparing how much information each response
  category provides (e.g., high-confidence suspect IDs
  vs. low-confidence IDs).
- **PPV range** is useful when false-identification rates are uncertain
  and lineup size corrections materially affect the PPV estimate.

These tools are intended to complement ROC/CAC analyses, not replace
them.
