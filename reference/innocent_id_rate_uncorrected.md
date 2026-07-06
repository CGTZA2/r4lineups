# Compute Innocent Suspect ID Rate with No Correction

Estimates innocent-suspect identification rate assuming all mistaken IDs
are innocent-suspect IDs (no correction for lineup size).

## Usage

``` r
innocent_id_rate_uncorrected(error_rate)
```

## Arguments

- error_rate:

  Numeric. Overall mistaken identification rate from culprit-absent
  lineups.

## Value

Numeric. Estimated innocent-suspect identification rate (equal to
error_rate).

## Details

This represents the "worst case" estimate, treating every mistaken ID as
an innocent-suspect identification. Provides an upper bound on
innocent-suspect risk. The formula is simply: innocent_id_rate =
error_rate

## References

Fitzgerald, R. J., Tredoux, C. G., & Juncu, S. (2023). Estimation of
eyewitness error rates in fair and biased lineups. *Law and Human
Behavior*.

## Examples

``` r
# Worst case: all mistaken IDs treated as innocent-suspect IDs
innocent_id_rate_uncorrected(0.30)
#> [1] 0.3
```
