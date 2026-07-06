# Plot Calibration Curves by Condition

Creates a faceted or overlaid plot comparing calibration across
conditions.

## Usage

``` r
make_calibration_by_condition_gg(
  cal_by_cond_obj,
  facet = TRUE,
  show_stats = TRUE
)
```

## Arguments

- cal_by_cond_obj:

  List output from make_calibration_by_condition()

- facet:

  Logical. If TRUE (default), creates faceted plot. If FALSE, overlays
  curves.

- show_stats:

  Logical. Whether to display statistics (default = TRUE when faceted)

## Value

A ggplot2 object

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
make_calibration_by_condition_gg(by_cond)

```
