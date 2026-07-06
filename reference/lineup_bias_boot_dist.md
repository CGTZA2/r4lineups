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

A numeric vector of bootstrap estimates.

## Examples

``` r
vec <- round(runif(200, 1, 6))
dist <- lineup_bias_boot_dist(vec, target_pos = 3, k = 6, R = 500)
```
