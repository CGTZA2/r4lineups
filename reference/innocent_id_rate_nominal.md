# Compute Innocent Suspect ID Rate with Nominal Size Correction

Estimates innocent-suspect identification rate using nominal lineup
size, assuming all lineup members are equally plausible (perfectly fair
lineup).

## Usage

``` r
innocent_id_rate_nominal(error_rate, lineup_size)
```

## Arguments

- error_rate:

  Numeric. Overall mistaken identification rate from culprit-absent
  lineups.

- lineup_size:

  Integer. Nominal number of people in the lineup.

## Value

Numeric. Estimated innocent-suspect identification rate.

## Details

This represents the "best case" estimate assuming perfect lineup
fairness. The formula is: innocent_id_rate = error_rate / lineup_size

## References

Fitzgerald, R. J., Tredoux, C. G., & Juncu, S. (2023). Estimation of
eyewitness error rates in fair and biased lineups. *Law and Human
Behavior*.

## Examples

``` r
# 30% mistaken ID rate in a fair 6-person lineup
innocent_id_rate_nominal(0.30, 6)
#> [1] 0.05
```
