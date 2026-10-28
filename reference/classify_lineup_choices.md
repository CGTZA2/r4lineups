# Classify Raw Lineup Choices into Identification Outcomes

Derives an identification outcome — suspect identification, foil
identification, or rejection — from the raw record of which lineup
member each witness actually chose.

## Usage

``` r
classify_lineup_choices(
  data,
  member_col = "lineup_member",
  suspect_col = "suspect_pos",
  into = "identification",
  k = NULL,
  overwrite = FALSE
)
```

## Arguments

- data:

  A data frame of lineup trials, one row per witness.

- member_col:

  Name of the column holding the position of the lineup member chosen,
  with `0` (or `NA`) denoting a rejection. Defaults to
  `"lineup_member"`.

- suspect_col:

  Either the name of a column holding the suspect's position for each
  trial, or a single number giving a suspect position common to all
  trials. Defaults to `"suspect_pos"`.

- into:

  Name of the column to write the derived outcome into. Defaults to
  `"identification"`.

- k:

  Optional lineup size. When supplied, choices are checked to lie in
  `0:k` and the positions of any null foils are attached to the result
  as the `"null_foils"` attribute.

- overwrite:

  Logical. Whether to overwrite `into` if it already exists. Defaults to
  `FALSE`, which raises an error instead.

## Value

`data` with the derived outcome column added. If `k` was supplied, the
`"null_foils"` attribute gives the positions that no witness chose.

## Details

Lineup data should record *which member* a witness chose, not merely
whether the choice happened to fall on the suspect. Foils are not
interchangeable: they differ in how plausible they are, and a foil that
no witness ever chooses is not a functioning alternative to the suspect.
Storing only a collapsed "suspect / filler / reject" label discards the
information needed to see that.

This function treats the collapsed label as *derived*. Raw data keeps
the chosen position; `classify_lineup_choices()` produces the label on
demand, so both are available and the two cannot drift apart.

The outcome is `"suspect"` when the chosen member is the suspect,
`"reject"` when no member was chosen, and `"filler"` otherwise. These
are the labels used throughout the package, so the result can be passed
straight to
[`make_roc`](https://cgtza2.github.io/r4lineups/reference/make_roc.md),
[`make_cac`](https://cgtza2.github.io/r4lineups/reference/make_cac.md)
and the other analysis functions.

## References

Malpass, R. S. (1981). Effective size and defendant bias in eyewitness
identification lineups. *Law and Human Behavior, 5*(4), 299-309.

Tredoux, C. G. (1998). Statistical inference on measures of lineup
fairness. *Law and Human Behavior, 22*(2), 217-237.

## See also

[`lineup_table`](https://cgtza2.github.io/r4lineups/reference/lineup_table.md)
for a tabulation that shows null foils, and
[`simulate_lineup_data`](https://cgtza2.github.io/r4lineups/reference/simulate_lineup_data.md),
which records `lineup_member` directly.

## Examples

``` r
# Raw choices from an 8-person lineup in which the suspect stands at
# position 3; 0 records a rejection
trials <- data.frame(
  lineup_member = c(3, 0, 5, 3, 1, 0, 3, 7),
  suspect_pos   = 3
)

classify_lineup_choices(trials, k = 8)
#>   lineup_member suspect_pos identification
#> 1             3           3        suspect
#> 2             0           3         reject
#> 3             5           3         filler
#> 4             3           3        suspect
#> 5             1           3         filler
#> 6             0           3         reject
#> 7             3           3        suspect
#> 8             7           3         filler

# A suspect position common to every trial can be given as a number
classify_lineup_choices(
  data.frame(lineup_member = c(2, 0, 4)),
  suspect_col = 2
)
#>   lineup_member identification
#> 1             2        suspect
#> 2             0         reject
#> 3             4         filler
```
