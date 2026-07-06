# Bootstrap Distribution of Effective Size

Computes a bootstrap distribution for effective size using either
Tredoux's E' or Malpass's adjusted E.

## Usage

``` r
esize_boot_dist(
  lineup_input,
  k,
  metric = c("tredoux", "malpass"),
  input_type = c("vector", "table"),
  R = 1000
)
```

## Arguments

- lineup_input:

  A numeric lineup vector or a lineup table.

- k:

  Nominal lineup size (suspect + fillers).

- metric:

  Either "tredoux" or "malpass".

- input_type:

  Either "vector" or "table".

- R:

  Number of bootstrap resamples.

## Value

A numeric vector of bootstrap estimates.

## Examples

``` r
vec <- round(runif(200, 1, 6))
dist <- esize_boot_dist(vec, k = 6, metric = "tredoux", R = 500)
```
