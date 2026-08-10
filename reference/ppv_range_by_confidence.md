# Compute PPV Range by Confidence (All Three Corrections)

Calculates PPV for each confidence level using all three correction
methods (nominal, effective, none) to show the range of plausible PPV
estimates.

## Usage

``` r
ppv_range_by_confidence(
  data,
  lineup_size = 6,
  confidence_bins = NULL,
  effective_size_data = NULL
)
```

## Arguments

- data:

  A dataframe with columns: target_present, identification, confidence

- lineup_size:

  Integer. Nominal number of people in lineup (default = 6)

- confidence_bins:

  Numeric vector of bin edges (optional)

- effective_size_data:

  Optional pre-computed effective sizes

## Value

A list of class "lineup_ppv_range" containing:

- ppv_range_data: Dataframe with all three PPV estimates per confidence
  level

- ppv_nominal: Nominal correction results

- ppv_effective: Effective size correction results

- ppv_none: No correction results

- lineup_size: Nominal lineup size

## Details

This function provides the full PPV range recommended by Fitzgerald et
al. (2023):

- **Nominal** (upper bound): Assumes perfectly fair lineup

- **Effective**: Accounts for lineup bias via effective size

- **None** (lower bound): Assumes worst-case (all errors are
  innocent-suspect)

The range between nominal and none represents the uncertainty in PPV due
to unknown lineup fairness conditions.

## References

Fitzgerald, R. J., Tredoux, C. G., & Juncu, S. (2023). Estimation of
eyewitness error rates in fair and biased lineups. *Law and Human
Behavior*.

## Examples

``` r
data(lineup_example)
# Compute full PPV range
ppv_range <- ppv_range_by_confidence(lineup_example,
                                     confidence_bins = c(0, 60, 80, 100))
print(ppv_range)
#> 
#> === Lineup PPV Range Analysis ===
#> 
#> Lineup size: 6 
#> 
#> Overall PPV by correction method:
#>   Nominal (best-case): 0.595 
#>   Effective (realistic): 0.595 
#>   None (worst-case): 0.595 
#> 
#> PPV range by confidence level:
#>  confidence ppv_nominal ppv_effective ppv_none effective_size error_rate
#>      [0,60]       0.500         0.500    0.500             NA      0.265
#>     (60,80]       0.630         0.630    0.630             NA      1.000

# Access individual corrections
ppv_range$ppv_nominal$overall_ppv
#> [1] 0.5945946
ppv_range$ppv_effective$overall_ppv
#> [1] 0.5945946
ppv_range$ppv_none$overall_ppv
#> [1] 0.5945946
```
