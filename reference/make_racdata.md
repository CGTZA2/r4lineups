# Compute RAC (Response Time-Accuracy Characteristic) Data

Computes response time-accuracy data for lineup identifications
following the approach described in Seale-Carlisle et al. (2019) and
implemented in pyWitness (Mickes et al., 2024). RAC analysis shows the
relationship between response time and accuracy for suspect
identifications.

## Usage

``` r
make_racdata(data, lineup_size = 6, time_bins = NULL)
```

## Arguments

- data:

  A dataframe with the following columns:

  - target_present: Logical. TRUE if guilty suspect in lineup

  - identification: Character. "suspect", "filler", or "reject"

  - response_time: Numeric. Response time in milliseconds (or seconds)

- lineup_size:

  Integer. Number of people in lineup (default = 6)

- time_bins:

  Numeric vector of bin edges for grouping response times (e.g., c(0,
  5000, 10000, 15000, 20000) creates bins in milliseconds). If NULL,
  uses individual response time values (not recommended for continuous
  data).

## Value

A list containing:

- rac_data: Dataframe with response time bins and accuracy

- n_ids_per_bin: Number of suspect IDs in each time bin

- overall_accuracy: Overall accuracy across all suspect IDs

## Details

RAC analysis computes, for each response time bin: \$\$Accuracy =
\frac{Correct Suspect IDs}{Correct Suspect IDs + Incorrect Suspect
IDs}\$\$

Only suspect IDs are included (filler IDs and rejections are ignored).
For target-absent lineups with no designated innocent suspect, filler
IDs are divided by lineup size to estimate incorrect suspect IDs.

RAC analysis is useful for examining the speed-accuracy tradeoff in
eyewitness identifications. Faster responses typically indicate stronger
memory, and RAC curves often show higher accuracy for faster response
times.

## References

Seale-Carlisle, T. M., Colloff, M. F., Flowe, H. D., Wells, W., Wixted,
J. T., & Mickes, L. (2019). Confidence and response time as indicators
of eyewitness identification accuracy in the lab and in the real world.
*Journal of Applied Research in Memory and Cognition, 8*(4), 420-428.

Mickes, L., Seale-Carlisle, T. M., Chen, X., & Boogert, S. (2024).
pyWitness 1.0: A python eyewitness identification analysis toolkit.
*Behavior Research Methods, 56*, 1533-1550.

## Examples

``` r
data <- create_example_lineup_data(n_trials = 200,
                                   include_response_time = TRUE,
                                   seed = 123)
rac <- make_racdata(data, time_bins = c(0, 4000, 8000, 12000, 20000))
rac$rac_data
#> # A tibble: 4 × 7
#>   response_time   mean_time n_correct n_incorrect n_total accuracy      se
#>   <chr>               <dbl>     <int>       <dbl>   <dbl>    <dbl>   <dbl>
#> 1 [0,4e+03]           2671.        24        5.5    29.5     0.814  0.0717
#> 2 (4e+03,8e+03]       5924.        35        9.67   44.7     0.784  0.0616
#> 3 (8e+03,1.2e+04]     8758.         3        2.83    5.83    0.514  0.207 
#> 4 (1.2e+04,2e+04]      NaN          0        0       0      NA     NA     
```
