# Compute and Plot CAC for Lineup Identification

Main function to compute and plot CAC (Confidence-Accuracy
Characteristic) for eyewitness lineup data.

## Usage

``` r
make_cac(data, lineup_size = 6, confidence_bins = NULL, show_plot = TRUE, ...)
```

## Arguments

- data:

  A dataframe with columns: target_present, identification, confidence

- lineup_size:

  Integer. Number of people in lineup (default = 6)

- confidence_bins:

  Numeric vector of bin edges (optional)

- show_plot:

  Logical. Whether to display the plot (default = TRUE)

- ...:

  Additional arguments passed to make_cac_gg()

## Value

A list containing CAC data and plot

## Examples

``` r
data(lineup_example)
cac_result <- make_cac(lineup_example, confidence_bins = c(0, 60, 80, 100))
print(cac_result)
#> 
#> === Lineup CAC Analysis ===
#> 
#> Overall Accuracy: 0.759 
#> Total Suspect IDs: 79 
#> Lineup size: 6 
#> 
#> CAC Data:
#> # A tibble: 3 × 6
#>   confidence n_correct n_incorrect n_total accuracy     se
#>   <chr>          <int>       <dbl>   <dbl>    <dbl>  <dbl>
#> 1 [0,60]             5        7.83    12.8    0.390 0.136 
#> 2 (60,80]           17       11.2     28.2    0.604 0.0922
#> 3 (80,100]          38        0       38      1     0     
#> 
#> Plot available in $plot
```
