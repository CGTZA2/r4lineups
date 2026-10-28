# Example Lineup Identification Data

A dataset containing simulated eyewitness lineup identification data
with target-present and target-absent lineups, including confidence
ratings.

## Usage

``` r
lineup_example
```

## Format

A data frame with 1400 rows and 5 variables:

- target_present:

  Logical indicating whether the guilty suspect was in the lineup

- lineup_member:

  Integer position of the lineup member chosen, with 0 for a rejection

- suspect_pos:

  Integer position occupied by the suspect (1 throughout)

- identification:

  Character indicating the witness decision: "suspect", "filler", or
  "reject"

- confidence:

  Numeric confidence rating (0-100 scale)

## Source

Simulated with
[`simulate_lineup_data`](https://cgtza2.github.io/r4lineups/reference/simulate_lineup_data.md).
The generating script is `data-raw/lineup_example.R`, which carries the
acceptance criteria the dataset must satisfy as explicit checks.

## Details

This dataset is used for demonstrating ROC, CAC, and EIG analyses in the
r4lineups package. It contains 700 target-present and 700 target-absent
lineups of size 6.

`lineup_member` records *which* member each witness chose, so foils keep
their separate identities and are not treated as interchangeable. They
are not merely simulated separately: each foil is given its own mean
memory strength, so some are strong alternatives to the suspect and
others are chosen only rarely. In this dataset the five foils attract
roughly 248, 203, 115, 53 and 10 identifications respectively.

`identification` is derived from `lineup_member` and `suspect_pos`, and
can be recovered exactly with
[`classify_lineup_choices`](https://cgtza2.github.io/r4lineups/reference/classify_lineup_choices.md);
it is retained because the analysis functions in this package take it
directly.

High confidence is strongly diagnostic here but deliberately *not*
perfect: a small number of innocent suspects are identified at the
highest confidence levels, as happens in real cases. Data in which no
innocent suspect is ever identified with high confidence produces an
infinite likelihood ratio and degenerate prior-posterior curves.

## See also

[`classify_lineup_choices`](https://cgtza2.github.io/r4lineups/reference/classify_lineup_choices.md),
[`lineup_table`](https://cgtza2.github.io/r4lineups/reference/lineup_table.md)

## Examples

``` r
data(lineup_example)
str(lineup_example)
#> 'data.frame':    1400 obs. of  5 variables:
#>  $ target_present: logi  FALSE FALSE TRUE TRUE TRUE FALSE ...
#>  $ lineup_member : int  2 3 2 1 1 0 1 1 1 4 ...
#>  $ suspect_pos   : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ identification: chr  "filler" "filler" "filler" "suspect" ...
#>  $ confidence    : num  40 50 40 80 60 20 60 70 20 50 ...
table(lineup_example$identification, lineup_example$target_present)
#>          
#>           FALSE TRUE
#>   filler    411  218
#>   reject    201   67
#>   suspect    88  415

# Which lineup members were chosen, and were any never chosen?
lineup_table(lineup_example$lineup_member[lineup_example$lineup_member > 0],
             k = 6, suspect_pos = 1)
#> Lineup choices (k = 6, N = 1132)
#> 
#>  pos:  1*    2    3    4   5   6
#>  cnt: 503  248  203  115  53  10
#> 
#>  * = nominated suspect
```
