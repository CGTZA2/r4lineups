# Compute and Plot DPP

Main wrapper function to compute and plot DPP analysis.

## Usage

``` r
compute_dpp(data, lineup_size = 6, show_plot = TRUE, ...)
```

## Arguments

- data:

  Dataframe with standard lineup format

- lineup_size:

  Integer. Lineup size (default = 6)

- show_plot:

  Logical. Whether to display plot (default = TRUE)

- ...:

  Additional arguments passed to plot_dpp()

## Value

A list containing the DPP result, raw AUC gap, AUC values, ROC data,
perfect-performance ROC data, maximum false-alarm rate, and plot.

## Examples

``` r
data(lineup_example)
dpp_result <- compute_dpp(lineup_example)

print(dpp_result)
#> 
#> === Deviation from Perfect Performance (DPP) ===
#> 
#> DPP:                 0.5757 
#>   (0 = perfect performance, 1 = worst performance)
#> 
#> Area under observed ROC:   0.0533 
#> Area under perfect ROC:    0.1257 
#> Raw AUC gap:               0.0724 
#> Maximum FA rate observed:  0.1257 
#> 
#> ROC Data:
#> # A tibble: 10 × 5
#>    confidence correct_id_rate false_id_rate n_correct_ids n_false_ids
#>         <dbl>           <dbl>         <dbl>         <dbl>       <dbl>
#>  1         19          0            0                   0           0
#>  2        100          0.0971       0                  68           0
#>  3         90          0.143        0.00571           100           4
#>  4         80          0.189        0.0143            132          10
#>  5         70          0.26         0.0229            182          16
#>  6         60          0.351        0.03              246          21
#>  7         50          0.436        0.0543            305          38
#>  8         40          0.507        0.0771            355          54
#>  9         30          0.556        0.103             389          72
#> 10         20          0.593        0.126             415          88
#> 
#> Plot: save this object first, then extract it with x$plot
```
