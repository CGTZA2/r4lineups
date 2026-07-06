# Compute Innocent Suspect ID Rate with Effective Size Correction

Estimates innocent-suspect identification rate using effective lineup
size, accounting for implausible fillers and lineup bias.

## Usage

``` r
innocent_id_rate_effective(error_rate, effective_size)
```

## Arguments

- error_rate:

  Numeric. Overall mistaken identification rate from culprit-absent
  lineups.

- effective_size:

  Numeric. Effective size computed from the distribution of mistaken IDs
  (typically via
  [`esize_T`](https://cgtza2.github.io/r4lineups/reference/esize_T.md)).

## Value

Numeric. Estimated innocent-suspect identification rate.

## Details

This represents a more realistic estimate when lineup fairness is
uncertain. Uses the distribution of mistaken IDs to estimate the number
of plausible lineup members. The formula is: innocent_id_rate =
error_rate / effective_size

When effective size \< nominal size, this indicates lineup bias and
increases the estimated innocent-suspect risk.

## References

Fitzgerald, R. J., Tredoux, C. G., & Juncu, S. (2023). Estimation of
eyewitness error rates in fair and biased lineups. *Law and Human
Behavior*.

Tredoux, C. G. (1998). Statistical inference on measures of lineup
fairness. *Law and Human Behavior, 22*(2), 217-237.

## Examples

``` r
# 30% mistaken ID rate with an effective size of 4.2
innocent_id_rate_effective(0.30, 4.2)
#> [1] 0.07142857
```
