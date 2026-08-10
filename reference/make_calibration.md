# Compute and Plot Calibration for Lineup Identification

Main function to compute and plot calibration analysis for eyewitness
lineup data.

## Usage

``` r
make_calibration(
  data,
  confidence_bins = NULL,
  choosers_only = TRUE,
  lineup_size = 6,
  show_plot = TRUE,
  confidence_scale = c("auto", "0-1", "0-100"),
  ...
)
```

## Arguments

- data:

  A dataframe with columns: target_present, identification, confidence

- confidence_bins:

  Numeric vector of bin edges (optional)

- choosers_only:

  Logical. Whether to analyze only suspect IDs (default = TRUE)

- lineup_size:

  Integer. Number of people in lineup (default = 6)

- show_plot:

  Logical. Whether to display the plot (default = TRUE)

- confidence_scale:

  How the confidence scale is interpreted: "auto" (default), "0-1", or
  "0-100". See
  [`make_calibration_data`](https://cgtza2.github.io/r4lineups/reference/make_calibration_data.md).

- ...:

  Additional arguments passed to make_calibration_gg()

## Value

A list containing calibration data, statistics, and plot

## Examples

``` r
set.seed(123)
n <- 200
lineup_data <- data.frame(
  target_present = rep(c(TRUE, FALSE), each = n / 2),
  identification = sample(c("suspect", "filler", "reject"), n,
                          replace = TRUE, prob = c(0.5, 0.25, 0.25)),
  confidence = round(runif(n, 0, 100))
)
cal_result <- make_calibration(lineup_data,
                               confidence_bins = c(0, 60, 80, 100))
print(cal_result)
#> 
#> === Lineup Calibration Analysis ===
#> 
#> Analysis type: Choosers only (suspect IDs)
#> Total N: 103 
#> 
#> Calibration Statistics:
#>   C (Calibration):         0.0521 
#>   O/U (Over/Under):        -0.0149 
#>   NRI (Resolution):        0.0096 
#> 
#> Overall Performance:
#>   Mean Accuracy:      0.515 
#>   Mean Confidence:    0.500 
#> 
#> Calibration Data by Bin:
#> # A tibble: 3 × 7
#>   bin          n mean_confidence accuracy n_correct n_incorrect
#>   <chr>    <dbl>           <dbl>    <dbl>     <dbl>       <dbl>
#> 1 [0,60]      61            28.2    0.475        29          32
#> 2 (60,80]     22            72.0    0.591        13           9
#> 3 (80,100]    20            92.0    0.55         11           9
#> # ℹ 1 more variable: mean_confidence_prop <dbl>
#> 
#> Plot available in $plot
```
