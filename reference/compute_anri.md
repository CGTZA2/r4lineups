# Compute ANRI (Adjusted Normalized Resolution Index)

Computes ANRI, a bias-corrected version of NRI following Yaniv et al.
(1991). ANRI adjusts NRI for the number of confidence bins and sample
size, providing more accurate estimates especially when J (bins) is
small relative to N.

## Usage

``` r
compute_anri(
  data,
  confidence_bins,
  choosers_only = TRUE,
  lineup_size = 6,
  confidence_scale = c("auto", "0-1", "0-100")
)
```

## Arguments

- data:

  A dataframe with the following columns:

  - target_present: Logical. TRUE if guilty suspect in lineup

  - identification: Character. "suspect", "filler", or "reject"

  - confidence: Numeric. Confidence rating

- confidence_bins:

  Numeric vector of bin edges (required for ANRI)

- choosers_only:

  Logical. Whether to analyze only suspect IDs (default = TRUE)

- lineup_size:

  Integer. Lineup size (default = 6)

- confidence_scale:

  How the confidence scale is interpreted: "auto" (default), "0-1", or
  "0-100". See
  [`make_calibration_data`](https://cgtza2.github.io/r4lineups/reference/make_calibration_data.md).

## Value

A list containing:

- anri: Adjusted Normalized Resolution Index

- nri: Original Normalized Resolution Index (unadjusted)

- n_total: Total sample size

- n_bins: Number of confidence bins

- calibration_data: Per-bin accuracy and sample sizes

- overall_accuracy: Mean accuracy

## Details

ANRI corrects NRI for small-sample and small-bin-count bias:

\$\$NRI = \frac{\frac{1}{N} \sum\_{j=1}^{J} n_j (a_j -
\bar{a})^2}{\bar{a}(1-\bar{a})}\$\$

\$\$ANRI = \frac{N \cdot NRI - J + 1}{N - J + 1}\$\$

Where:

- N = total sample size

- J = number of bins

- n_j = sample size in bin j

- a_j = accuracy (proportion correct) in bin j

- a_bar = overall accuracy

\*\*When to use ANRI vs NRI:\*\*

- Use ANRI when J is small (e.g., 3-5 bins)

- Use ANRI when N is modest (e.g., \< 200)

- Use ANRI when comparing across different numbers of bins

- NRI is asymptotically unbiased as N approaches infinity

ANRI provides a less biased estimate of the population resolution,
making it more appropriate for hypothesis testing and group comparisons.

## References

Yaniv, I., Yates, J. F., & Smith, J. E. K. (1991). Measures of
discrimination skill in probabilistic judgment. *Psychological Bulletin,
110*(3), 611-617.

Juslin, P., Olsson, N., & Winman, A. (1996). Calibration and
diagnosticity of confidence in eyewitness identification. *Journal of
Experimental Psychology: Learning, Memory, and Cognition, 22*(5),
1304-1316.

## Examples

``` r
set.seed(123)
n <- 200
data <- data.frame(
  target_present = rep(c(TRUE, FALSE), each = n / 2),
  identification = sample(c("suspect", "filler", "reject"), n,
                          replace = TRUE, prob = c(0.5, 0.25, 0.25)),
  confidence = round(runif(n, 0, 100))
)
result <- compute_anri(data, confidence_bins = seq(0, 100, 20))
result$anri
#> [1] -0.01987784
```
