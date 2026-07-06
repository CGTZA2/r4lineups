# Plot Bootstrap Distribution of ANRI Difference

Creates a histogram of the bootstrap distribution of the difference
between two groups.

## Usage

``` r
plot_anri_difference_distribution(compare_obj)
```

## Arguments

- compare_obj:

  List output from compare_anri()

## Value

A ggplot2 object

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
plot_anri_difference_distribution(cmp)

# }
```
