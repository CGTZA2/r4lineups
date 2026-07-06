# Bootstrap Confidence Intervals for ANRI

Computes bootstrap confidence intervals around ANRI estimates using
percentile method.

## Usage

``` r
bootstrap_anri(
  data,
  confidence_bins,
  choosers_only = TRUE,
  lineup_size = 6,
  n_bootstrap = 1000,
  conf_level = 0.95,
  seed = NULL,
  confidence_scale = c("auto", "0-1", "0-100")
)
```

## Arguments

- data:

  Dataframe with standard lineup format

- confidence_bins:

  Numeric vector of bin edges

- choosers_only:

  Logical. Whether to analyze only suspect IDs (default = TRUE)

- lineup_size:

  Integer. Lineup size (default = 6)

- n_bootstrap:

  Integer. Number of bootstrap replications (default = 1000)

- conf_level:

  Numeric. Confidence level for CIs (default = 0.95)

- seed:

  Integer. Random seed for reproducibility (default = NULL)

- confidence_scale:

  How the confidence scale is interpreted: "auto" (default), "0-1", or
  "0-100". See
  [`make_calibration_data`](https://cgtza2.github.io/r4lineups/reference/make_calibration_data.md).

## Value

A list containing:

- anri: Point estimate of ANRI

- nri: Point estimate of NRI

- ci_lower: Lower bound of bootstrap CI

- ci_upper: Upper bound of bootstrap CI

- conf_level: Confidence level used

- n_bootstrap: Number of bootstrap replications

- bootstrap_distribution: Vector of bootstrap ANRI values

## Details

Bootstrap resampling procedure: 1. Resample observations with
replacement 2. Compute ANRI for each bootstrap sample 3. Construct
percentile-based confidence interval

The percentile method uses the empirical quantiles of the bootstrap
distribution. For a 95

## References

Efron, B., & Tibshirani, R. J. (1994). *An Introduction to the
Bootstrap*. Chapman & Hall/CRC.

## Examples

``` r
# \donttest{
set.seed(123)
n <- 200
data <- data.frame(
  target_present = rep(c(TRUE, FALSE), each = n / 2),
  identification = sample(c("suspect", "filler", "reject"), n,
                          replace = TRUE, prob = c(0.5, 0.25, 0.25)),
  confidence = round(runif(n, 0, 100))
)
boot_result <- bootstrap_anri(data, confidence_bins = seq(0, 100, 20),
                              n_bootstrap = 200, seed = 1)
c(boot_result$ci_lower, boot_result$ci_upper)
#> [1] -0.03579564  0.13450153
# }
```
