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
#> DPP:                 0.5396 
#>   (0 = perfect performance, 1 = worst performance)
#> 
#> Area under observed ROC:   0.0875 
#> Area under perfect ROC:    0.1900 
#> Raw AUC gap:               0.1025 
#> Maximum FA rate observed:  0.1900 
#> 
#> ROC Data:
#> # A tibble: 10 × 5
#>    confidence correct_id_rate false_id_rate n_correct_ids n_false_ids
#>         <dbl>           <dbl>         <dbl>         <dbl>       <dbl>
#>  1        100            0.17         0                17         0  
#>  2         90            0.38         0                38         0  
#>  3         19            0            0                 0         0  
#>  4         80            0.48         0.06             48         6  
#>  5         70            0.55         0.112            55        11.2
#>  6         60            0.6          0.133            60        13.3
#>  7         50            0.6          0.18             60        18  
#>  8         40            0.6          0.19             60        19  
#>  9         30            0.6          0.19             60        19  
#> 10         20            0.6          0.19             60        19  
#> 
#> Plot available in $plot
```
