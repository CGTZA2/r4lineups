# Bootstrap Distribution of Lineup Bias (Target Proportion)

Computes a bootstrap distribution for the target-position proportion.

## Usage

``` r
lineup_bias_boot_dist(
  lineup_input,
  target_pos,
  k,
  input_type = c("vector", "table"),
  R = 1000
)
```

## Arguments

- lineup_input:

  A numeric lineup vector or a lineup table.

- target_pos:

  Target position in the lineup (scalar).

- k:

  Nominal lineup size (suspect + fillers).

- input_type:

  Either "vector" or "table".

- R:

  Number of bootstrap resamples.

## Value

A numeric vector of bootstrap estimates, carrying an `"n"` attribute
equal to the number of lineup choices resampled. Because each estimate
is a count divided by `n`, the estimates fall on a discrete lattice with
spacing `1/n`;
[`plot_lineup_bias_distribution`](https://cgtza2.github.io/r4lineups/reference/plot_lineup_bias_distribution.md)
uses the attribute to bin at that resolution.

## Examples

``` r
vec <- round(runif(200, 1, 6))
dist <- lineup_bias_boot_dist(vec, target_pos = 3, k = 6, R = 500)
```
