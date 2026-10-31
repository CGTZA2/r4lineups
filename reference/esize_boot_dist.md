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
  R = 1000,
  unbiased = FALSE
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

- unbiased:

  Logical. For `metric = "tredoux"`, bootstrap the small-sample
  corrected E' of
  [`esize_T`](https://cgtza2.github.io/r4lineups/reference/esize_T.md)`(unbiased = TRUE)`
  instead of the plug-in estimate. Default `FALSE`.

## Value

A numeric vector of bootstrap estimates, carrying an `"n"` attribute
equal to the number of lineup choices resampled. With `unbiased = TRUE`,
resamples in which every choice fell on a different member have an
unbounded corrected estimate; they are retained as `Inf`, with a
warning, and their number is recorded in an `"n_undefined"` attribute.

## Details

Percentile or BCa intervals from the plug-in E' bootstrap are not valid
tests of exact uniform fairness. The population maximum E'=k is a
nonregular boundary. If the sample size is not a multiple of k, even the
largest attainable empirical E' is below k, so those intervals have zero
coverage at exact fairness. More bootstrap replicates cannot fix this.

## Examples

``` r
vec <- round(runif(200, 1, 6))
dist <- esize_boot_dist(vec, k = 6, metric = "tredoux", R = 500)
dist_corrected <- esize_boot_dist(vec, k = 6, R = 500, unbiased = TRUE)
```
