# Create Example Lineup Data

Generate example lineup identification data in standardized format.
Useful for testing, demonstrations, and learning r4lineups functions.

## Usage

``` r
create_example_lineup_data(
  n_trials = 100,
  prop_target_present = 0.5,
  include_confidence = TRUE,
  include_response_time = FALSE,
  seed = NULL
)
```

## Arguments

- n_trials:

  Integer. Number of trials to generate (default = 100)

- prop_target_present:

  Numeric. Proportion of target-present lineups (default = 0.5)

- include_confidence:

  Logical. Include confidence ratings (default = TRUE)

- include_response_time:

  Logical. Include response times (default = FALSE)

- seed:

  Integer. Random seed for reproducibility (default = NULL)

## Value

A lineup_data object with realistic example data

## Examples

``` r
# Basic example data
example_data <- create_example_lineup_data(n_trials = 200, seed = 123)
print(example_data)
#> 
#> === Standardized Lineup Data ===
#> 
#> Format: r4lineups standard format
#> Rows: 200 
#> Columns: participant_id, target_present, identification, confidence 
#> 
#> Target-present trials: 97 
#> Target-absent trials: 103 
#> 
#> Identifications:
#>   filler    : 62
#>   reject    : 64
#>   suspect   : 74
#> 
#> Confidence range: 1 to 5 
#> 
#> First 10 rows:
#>    participant_id target_present identification confidence
#> 1               1          FALSE         reject          4
#> 2               2           TRUE         reject          1
#> 3               3          FALSE         filler          3
#> 4               4           TRUE        suspect          4
#> 5               5           TRUE        suspect          5
#> 6               6          FALSE         filler          5
#> 7               7           TRUE        suspect          4
#> 8               8           TRUE        suspect          5
#> 9               9           TRUE        suspect          5
#> 10             10          FALSE         reject          1

# Use in analysis
roc <- make_roc(example_data, lineup_size = 6)
```
