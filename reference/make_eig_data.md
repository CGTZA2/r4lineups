# Prepare EIG Data from Lineup Identification

Prepares response category data for Expected Information Gain (EIG)
analysis following Starns et al. (2023). Creates response categories
from identification decisions and confidence levels.

## Usage

``` r
make_eig_data(data, confidence_bins = NULL, lineup_size = 6)
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

- lineup_size:

  Nominal lineup size. Used to split target-absent filler
  identifications when no designated innocent suspect is present.

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

Starns, J. J., Cohen, A. L., & Rotello, C. M. (2023). A complete method
for assessing the effectiveness of eyewitness identification procedures:
Expected information gain. *Psychological Review, 130*(3), 677–719.

## Examples

``` r
data(lineup_example)
eig_data <- make_eig_data(lineup_example, confidence_bins = c(0, 60, 80, 100))
eig_data$response_data
#> # A tibble: 7 × 7
#>   response         n_guilty n_innocent p_x_given_guilty p_x_given_innocent
#>   <chr>               <dbl>      <dbl>            <dbl>              <dbl>
#> 1 filler_(60,80]         40         59           0.0571            0.0843 
#> 2 filler_(80,100]        13         30           0.0186            0.0429 
#> 3 filler_[0,60]         165        322           0.236             0.46   
#> 4 reject_[0,60]          67        201           0.0957            0.287  
#> 5 suspect_(60,80]        82         12           0.117             0.0171 
#> 6 suspect_(80,100]      100          4           0.143             0.00571
#> 7 suspect_[0,60]        233         72           0.333             0.103  
#> # ℹ 2 more variables: identification <chr>, confidence_level <chr>
```
