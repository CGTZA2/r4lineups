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
#> DPP:                 0.4923 
#>   (0 = perfect performance, 1 = worst performance)
#> 
#> Area under observed ROC:   0.0761 
#> Area under perfect ROC:    0.1500 
#> Raw AUC gap:               0.0738 
#> Maximum FA rate observed:  0.1500 
#> 
#> ROC Data:
#> # A tibble: 10 × 5
#>    confidence correct_id_rate false_id_rate n_correct_ids n_false_ids
#>         <dbl>           <dbl>         <dbl>         <dbl>       <dbl>
#>  1         19            0             0                0           0
#>  2        100            0.17          0               17           0
#>  3         90            0.38          0               38           0
#>  4         80            0.48          0.06            48           6
#>  5         70            0.55          0.1             55          10
#>  6         60            0.6           0.11            60          11
#>  7         50            0.6           0.15            60          15
#>  8         40            0.6           0.15            60          15
#>  9         30            0.6           0.15            60          15
#> 10         20            0.6           0.15            60          15
#> 
#> Plot available in $plot
```
