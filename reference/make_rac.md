# Compute and Plot RAC for Lineup Identification

Main function to compute and plot RAC (Response Time-Accuracy
Characteristic) for eyewitness lineup data.

## Usage

``` r
make_rac(data, lineup_size = 6, time_bins = NULL, show_plot = TRUE, ...)
```

## Arguments

- data:

  A dataframe with columns: target_present, identification,
  response_time

- lineup_size:

  Integer. Number of people in lineup (default = 6)

- time_bins:

  Numeric vector of bin edges (recommended for continuous RT data)

- show_plot:

  Logical. Whether to display the plot (default = TRUE)

- ...:

  Additional arguments passed to make_rac_gg()

## Value

A list containing RAC data and plot

## Examples

``` r
# Example with binned response times (in milliseconds)
lineup_data <- create_example_lineup_data(n_trials = 200,
                                          include_response_time = TRUE,
                                          seed = 123)
rac_result <- make_rac(lineup_data,
                       time_bins = c(0, 5000, 10000, 15000, 20000))
print(rac_result)
#> 
#> === Lineup RAC Analysis ===
#> 
#> Overall Accuracy: 0.838 
#> Total Suspect IDs: 74 
#> Lineup size: 6 
#> 
#> RAC Data:
#> # A tibble: 4 × 7
#>   response_time   mean_time n_correct n_incorrect n_total accuracy      se
#>   <chr>               <dbl>     <int>       <int>   <int>    <dbl>   <dbl>
#> 1 [0,5e+03]           3193.        33           8      41    0.805  0.0619
#> 2 (5e+03,1e+04]       6632.        29           4      33    0.879  0.0568
#> 3 (1e+04,1.5e+04]      NaN          0           0       0   NA     NA     
#> 4 (1.5e+04,2e+04]      NaN          0           0       0   NA     NA     
#> 
#> Plot available in $plot
rac_result$plot
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_line()`).
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_text()`).

```
