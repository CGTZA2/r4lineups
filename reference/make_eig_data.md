# Prepare EIG Data from Lineup Identification

Prepares response category data for Expected Information Gain (EIG)
analysis following Starns et al. (2023). Creates response categories
from identification decisions and confidence levels.

## Usage

``` r
make_eig_data(data, confidence_bins = NULL)
```

## Arguments

- data:

  A dataframe with the following columns:

  - target_present: Logical. TRUE if guilty suspect in lineup

  - identification: Character. "suspect", "filler", or "reject"

  - confidence: Numeric. Confidence rating

- confidence_bins:

  Numeric vector of bin edges for grouping confidence (e.g., c(0, 60,
  80, 100) creates bins 0-60, 61-80, 81-100). If NULL, uses individual
  confidence levels.

## Value

A list containing:

- response_data: Dataframe with response categories and frequencies

- n_guilty: Total number of target-present lineups

- n_innocent: Total number of target-absent lineups

- confidence_bins: The binning specification used (if any)

## Details

Response categories are defined as combinations of identification
decision (suspect/filler/reject) and confidence level or bin. This
function counts the frequency of each response category separately for
target-present (guilty) and target-absent (innocent) lineups.

## References

Starns, J. J., Chen, T., & Staub, A. (2023). Assessing theoretical
conclusions via the data they should have produced: A priori comparison
of eyewitness identification decision processes using quantitative
predictions of the expected information gain. *Psychological Review*.

## Examples

``` r
data(lineup_example)
eig_data <- make_eig_data(lineup_example, confidence_bins = c(0, 60, 80, 100))
eig_data$response_data
#> # A tibble: 6 × 7
#>   response         n_guilty n_innocent p_x_given_guilty p_x_given_innocent
#>   <chr>               <dbl>      <dbl>            <dbl>              <dbl>
#> 1 filler_(60,80]          1          7             0.01               0.07
#> 2 filler_[0,60]          17         17             0.17               0.17
#> 3 reject_[0,60]          22         61             0.22               0.61
#> 4 suspect_(60,80]        17         10             0.17               0.1 
#> 5 suspect_(80,100]       38          0             0.38               0   
#> 6 suspect_[0,60]          5          5             0.05               0.05
#> # ℹ 2 more variables: identification <chr>, confidence_level <chr>
```
