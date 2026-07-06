# Plot ANRI Comparison Between Groups

Creates a visualization comparing ANRI estimates between two groups with
bootstrap confidence intervals.

## Usage

``` r
plot_anri_comparison(compare_obj)
```

## Arguments

- compare_obj:

  List output from compare_anri()

## Value

A ggplot2 object

## Details

Creates a point-and-interval plot showing ANRI estimates and bootstrap
CIs for each group. Helps visualize the magnitude and uncertainty of
group differences.

## Examples

``` r
# \donttest{
set.seed(123)
n <- 300
data <- data.frame(
  target_present = rep(c(TRUE, FALSE), each = n / 2),
  identification = sample(c("suspect", "filler", "reject"), n,
                          replace = TRUE, prob = c(0.5, 0.25, 0.25)),
  confidence = round(runif(n, 0, 100)),
  condition = rep(c("sequential", "simultaneous"), times = n / 2)
)
cmp <- compare_anri(data, group_var = "condition",
                    confidence_bins = seq(0, 100, 20),
                    n_bootstrap = 100, seed = 1)
plot_anri_comparison(cmp)

# }
```
