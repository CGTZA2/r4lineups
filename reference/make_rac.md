# Compute and Plot RAC for Lineup Identification

Main function to compute and plot RAC (Response Time-Accuracy
Characteristic) for eyewitness lineup data.

## Usage

``` r
make_rac(
  data,
  lineup_size = 6,
  time_bins = NULL,
  show_plot = TRUE,
  innocent_suspect = "auto",
  ...
)
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
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_line()`).
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_text()`).

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
#>   response_time mean_time n_correct n_incorrect n_total accuracy      se
#>   <chr>             <dbl>     <int>       <int>   <int>    <dbl>   <dbl>
#> 1 [0,5000]          3193.        33           8      41    0.805  0.0619
#> 2 (5000,10000]      6632.        29           4      33    0.879  0.0568
#> 3 (10000,15000]      NaN          0           0       0   NA     NA     
#> 4 (15000,20000]      NaN          0           0       0   NA     NA     
#> 
#> Plot: save this object first, then extract it with x$plot
rac_result$plot
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_line()`).
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_text()`).

```
