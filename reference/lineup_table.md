# Tabulate Lineup Choices, Including Null Foils

Builds a table of lineup choices with one entry for every lineup member,
including members that no mock witness chose.

## Usage

``` r
lineup_table(lineup_vec, k, suspect_pos = NULL)
```

## Arguments

- lineup_vec:

  A numeric vector of lineup choices.

- k:

  Number of members in the lineup (suspect plus foils). Must be
  specified by the user (scalar).

- suspect_pos:

  Optional position of the nominated suspect, marked in the printed
  output. Defaults to `NULL`, for lineups with no pre-specified suspect.

## Value

An object of class `lineup_table`, inheriting from `table`, with `k`
named entries. Because it inherits from `table` it can be passed to
[`esize_m`](https://cgtza2.github.io/r4lineups/reference/esize_m.md),
[`esize_T`](https://cgtza2.github.io/r4lineups/reference/esize_T.md) and
the other table-taking functions in this package unchanged.

## Details

Calling [`table()`](https://rdrr.io/r/base/table.html) directly on a
vector of lineup choices tabulates only the positions that were actually
chosen. A lineup member that no witness selected — a *null foil* — is
silently dropped, so the resulting table has fewer than `k` entries and
nothing in the output reveals that a member is missing. This matters
because null foils are precisely the members that contribute most to a
lineup being unfair: a foil nobody ever picks is not a functioning
alternative to the suspect.

`lineup_table()` tabulates over all `k` positions, so unchosen members
appear explicitly as zero counts, and its print method names them.

The measures in this package already handle null foils correctly when
given a complete table.
[`esize_m()`](https://cgtza2.github.io/r4lineups/reference/esize_m.md),
for instance, reindexes counts by position and deducts unchosen members
from the adjusted estimate. What `lineup_table()` adds is visibility,
not a change in the arithmetic.

## References

Malpass, R. S. (1981). Effective size and defendant bias in eyewitness
identification lineups. *Law and Human Behavior, 5*(4), 299-309.

Tredoux, C. G. (1998). Statistical inference on measures of lineup
fairness. *Law and Human Behavior, 22*(2), 217-237.

## See also

[`esize_m`](https://cgtza2.github.io/r4lineups/reference/esize_m.md),
[`esize_T`](https://cgtza2.github.io/r4lineups/reference/esize_T.md),
[`allprop`](https://cgtza2.github.io/r4lineups/reference/allprop.md)

## Examples

``` r
# A lineup in which position 4 was never chosen
lineup_vec <- rep(c(5, 3, 2, 1, 8, 6, 7), each = 4)

# table() drops the unchosen member: only 7 entries for an 8-person lineup
table(lineup_vec)
#> lineup_vec
#> 1 2 3 5 6 7 8 
#> 4 4 4 4 4 4 4 

# lineup_table() keeps it, and says so
lineup_table(lineup_vec, k = 8, suspect_pos = 3)
#> Lineup choices (k = 8, N = 28)
#> 
#>  pos: 1  2  3*  4  5  6  7  8
#>  cnt: 4  4   4  0  4  4  4  4
#> 
#>  * = nominated suspect
#> 
#>  Note: 1 null foil (position 4) received no choices.
#>  Unchosen members reduce effective size; see ?esize_m.

# Accepted by the effective size functions unchanged
esize_m(lineup_table(lineup_vec, k = 8), k = 8)
#> [1] 7
```
