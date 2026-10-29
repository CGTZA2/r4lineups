# Compute and Plot DPP

Main wrapper function to compute and plot DPP analysis.

## Usage

``` r
compute_dpp(
  data,
  lineup_size = 6,
  show_plot = TRUE,
  alpha = NULL,
  beta = NULL,
  innocent_suspect = "auto",
  ...
)
```

## Arguments

- data:

  Dataframe with standard lineup format

- lineup_size:

  Integer. Lineup size (default = 6)

- show_plot:

  Logical. Whether to display plot (default = TRUE)

- alpha, beta:

  Optional Equation 3 weights; see
  [`make_dpp`](https://cgtza2.github.io/r4lineups/reference/make_dpp.md).

- innocent_suspect:

  How innocent-suspect identifications in target-absent lineups are
  obtained. `"designated"`: the lineups had a designated innocent
  suspect, so target-absent `"suspect"` rows are counted directly, and
  zero such rows means a false-ID rate of zero. `"estimated"`: there was
  no designated innocent suspect, so the innocent-suspect count is
  estimated as filler IDs divided by `lineup_size`. `"auto"` (the
  default) chooses `"designated"` when any target-absent row is a
  suspect ID or the data have a non-missing `suspect_pos` column, and
  `"estimated"` otherwise, with a message.

- ...:

  Additional arguments passed to plot_dpp()

## Value

An object of class `"lineup_dpp"`: the
[`make_dpp()`](https://cgtza2.github.io/r4lineups/reference/make_dpp.md)
result plus the plot.

## Examples

``` r
data(lineup_example)
dpp_result <- compute_dpp(lineup_example, show_plot = FALSE)
print(dpp_result)
#> 
#> === Deviation from Perfect Performance (DPP) ===
#> 
#> Average DPP: 0.7002 
#>   (0 = perfect, 1 = chance, 2 = worst; Smith et al., 2019, Eq. 2)
#> 
#> Point of maximum utility: confidence >= 20 (DPP = 0.5329)
#> 
#> DPP by operating point:
#> # A tibble: 9 × 4
#>   confidence culprit_rate innocent_rate   dpp
#>        <dbl>        <dbl>         <dbl> <dbl>
#> 1        100       0.0971       0       0.903
#> 2         90       0.143        0.00571 0.863
#> 3         80       0.189        0.0143  0.826
#> 4         70       0.26         0.0229  0.763
#> 5         60       0.351        0.03    0.679
#> 6         50       0.436        0.0543  0.619
#> 7         40       0.507        0.0771  0.57 
#> 8         30       0.556        0.103   0.547
#> 9         20       0.593        0.126   0.533
#> 
#> Plot: save this object first, then extract it with x$plot
```
