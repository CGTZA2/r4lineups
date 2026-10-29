# Bootstrap pAUC Comparison

Bootstrap pAUC Comparison

## Usage

``` r
.bootstrap_pauc_comparison(
  data1,
  data2,
  lineup_size,
  max_false_id_rate,
  n_bootstrap,
  innocent_suspect = c("auto", "auto")
)
```

## Arguments

- data1:

  First dataset

- data2:

  Second dataset

- lineup_size:

  Lineup size

- max_false_id_rate:

  Maximum false ID rate

- n_bootstrap:

  Number of bootstrap samples

- innocent_suspect:

  Resolved innocent-suspect methods for the two conditions, reused in
  every replicate

## Value

List with bootstrap distributions
