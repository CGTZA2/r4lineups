# Compute Calibration Statistics by Condition

Computes calibration analysis separately for different experimental
conditions. Useful for examining how calibration varies across lineup
instructions, foil similarity, or other system/estimator variables.

## Usage

``` r
make_calibration_by_condition(
  data,
  condition_vars,
  confidence_bins = NULL,
  choosers_only = TRUE,
  lineup_size = 6,
  confidence_scale = c("auto", "0-1", "0-100")
)
```

## Arguments

- data:

  A dataframe with columns: target_present, identification, confidence,
  plus one or more condition variables

- condition_vars:

  Character vector of column names defining conditions (e.g.,
  c("instruction_type", "foil_similarity"))

- confidence_bins:

  Numeric vector of bin edges (optional)

- choosers_only:

  Logical. Whether to analyze only suspect IDs (default = TRUE)

- lineup_size:

  Integer. Number of people in lineup (default = 6)

- confidence_scale:

  How the confidence scale is interpreted: "auto" (default), "0-1", or
  "0-100". See
  [`make_calibration_data`](https://cgtza2.github.io/r4lineups/reference/make_calibration_data.md).

## Value

A list containing:

- by_condition: List of calibration results for each condition
  combination

- condition_summary: Dataframe summarizing C, O/U, NRI across conditions

## Details

This function splits the data by the specified condition variables and
computes calibration statistics separately for each combination. Useful
for examining questions like "Does calibration improve with biased vs
unbiased instructions?" or "How does foil similarity affect
over/underconfidence?"

## References

Brewer, N., & Wells, G. L. (2006). The confidence-accuracy relationship
in eyewitness identification: Effects of lineup instructions, foil
similarity, and target-absent base rates. *Journal of Experimental
Psychology: Applied, 12*(1), 11-30.

## Examples

``` r
set.seed(123)
n <- 300
data <- data.frame(
  target_present = rep(c(TRUE, FALSE), each = n / 2),
  identification = sample(c("suspect", "filler", "reject"), n,
                          replace = TRUE, prob = c(0.5, 0.25, 0.25)),
  confidence = round(runif(n, 0, 100)),
  instruction = rep(c("biased", "unbiased"), times = n / 2)
)
by_cond <- make_calibration_by_condition(data, condition_vars = "instruction",
                                         confidence_bins = c(0, 60, 80, 100))
by_cond$condition_summary
#> # A tibble: 2 × 7
#>   condition     n      C      OU    NRI overall_accuracy overall_confidence
#> * <chr>     <dbl>  <dbl>   <dbl>  <dbl>            <dbl>              <dbl>
#> 1 biased       79 0.0690  0.0495 0.0210            0.481              0.531
#> 2 unbiased     76 0.120  -0.0270 0.0262            0.526              0.499
```
