# Compute Calibration Statistics for Eyewitness Identification

Computes calibration analysis metrics following Juslin, Olsson, & Winman
(1996). Calibration analysis assesses the match between confidence and
accuracy, providing a more appropriate measure than simple
confidence-accuracy correlation.

## Usage

``` r
make_calibration_data(
  data,
  confidence_bins = NULL,
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

  Numeric vector of bin edges for grouping confidence (e.g., c(0, 60,
  80, 100) creates bins 0-60, 60-80, 80-100). If NULL, uses individual
  confidence levels.

- choosers_only:

  Logical. If TRUE (default), only analyze suspect identifications. If
  FALSE, analyze all responses (including fillers and rejections).

- lineup_size:

  Integer. Number of people in lineup (default = 6). Used for estimating
  incorrect suspect IDs from filler choices in target-absent lineups.

- confidence_scale:

  How the confidence scale is interpreted: "auto" (default; max \<= 1
  treated as 0-1, otherwise as 0-100, with a warning for ambiguous
  Likert/0-10 scales), "0-1", or "0-100". C and O/U require a 0-1/0-100
  probability scale.

## Value

A list containing:

- calibration_data: Dataframe with per-bin confidence, accuracy, and
  counts

- C: Calibration statistic (weighted mean squared difference between
  confidence and accuracy)

- OU: Over/underconfidence (mean confidence minus mean accuracy)

- NRI: Normalized Resolution Index (standardized within-person variance)

- overall_accuracy: Mean accuracy across all responses

- overall_confidence: Mean confidence across all responses

- n_total: Total number of responses analyzed

## Details

Calibration analysis distinguishes between:

- **Calibration (C)**: How well confidence matches accuracy (perfect =
  0)

- **Over/underconfidence (O/U)**: Overall bias in confidence judgments
  (0 = perfectly calibrated)

- **Resolution (NRI)**: Ability to discriminate correct from incorrect
  responses

The calibration statistic C measures the weighted mean squared deviation
between mean confidence and accuracy in each bin: \$\$C =
\sum\_{j=1}^{J} \frac{n_j}{N} (c_j - a_j)^2\$\$

The Normalized Resolution Index (NRI) captures how well confidence
discriminates correct from incorrect responses: \$\$NRI =
\frac{\frac{1}{N} \sum\_{j=1}^{J} n_j (a_j -
\bar{a})^2}{\bar{a}(1-\bar{a})}\$\$

When `choosers_only = TRUE`, only suspect identifications are included
(standard for eyewitness calibration analysis). When
`choosers_only = FALSE`, all responses are included with fillers and
rejections counted as incorrect.

## References

Juslin, P., Olsson, N., & Winman, A. (1996). Calibration and
diagnosticity of confidence in eyewitness identification: Comments on
what can be inferred from the low confidence-accuracy correlation.
*Journal of Experimental Psychology: Learning, Memory, and Cognition,
22*(5), 1304-1316.

Brewer, N., & Wells, G. L. (2006). The confidence-accuracy relationship
in eyewitness identification: Effects of lineup instructions, foil
similarity, and target-absent base rates. *Journal of Experimental
Psychology: Applied, 12*(1), 11-30.

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
cal <- make_calibration_data(data, confidence_bins = c(0, 60, 80, 100))
cal$C
#> [1] 0.05214408
cal$calibration_data
#> # A tibble: 3 × 7
#>   bin          n mean_confidence accuracy n_correct n_incorrect
#>   <chr>    <int>           <dbl>    <dbl>     <dbl>       <dbl>
#> 1 [0,60]      61            28.2    0.475        29          32
#> 2 (60,80]     22            72.0    0.591        13           9
#> 3 (80,100]    20            92.0    0.55         11           9
#> # ℹ 1 more variable: mean_confidence_prop <dbl>
```
