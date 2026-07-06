# Example Lineup Identification Data

A dataset containing simulated eyewitness lineup identification data
with target-present and target-absent lineups, including confidence
ratings.

## Usage

``` r
lineup_example
```

## Format

A data frame with 200 rows and 3 variables:

- target_present:

  Logical indicating whether the guilty suspect was in the lineup

- identification:

  Character indicating the witness decision: "suspect", "filler", or
  "reject"

- confidence:

  Numeric confidence rating (0-100 scale)

## Source

Simulated data for package examples

## Details

This dataset is used for demonstrating ROC, CAC, and EIG analyses in the
r4lineups package. It contains 100 target-present lineups and 100
target-absent lineups with realistic distributions of identification
decisions and confidence ratings.

## Examples

``` r
data(lineup_example)
str(lineup_example)
#> 'data.frame':    200 obs. of  3 variables:
#>  $ target_present: logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
#>  $ identification: chr  "suspect" "reject" "suspect" "filler" ...
#>  $ confidence    : num  90 50 90 50 50 90 70 40 90 60 ...
table(lineup_example$identification, lineup_example$target_present)
#>          
#>           FALSE TRUE
#>   filler     24   18
#>   reject     61   22
#>   suspect    15   60
```
