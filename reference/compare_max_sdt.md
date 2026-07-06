# Compare Two Nested MAX SDT Models

Performs a chi-squared difference test comparing a less constrained
(free) MAX SDT model to a more constrained (nested) model. Both models
must be fitted to the same data.

## Usage

``` r
compare_max_sdt(free, constrained)
```

## Arguments

- free:

  A `"max_sdt_fit"` object: the less constrained model.

- constrained:

  A `"max_sdt_fit"` object: the more constrained model. Must have fewer
  free parameters than `free`.

## Value

An object of class `"max_sdt_compare"` with the test result.

## Details

The chi-squared difference statistic is: \$\$\Delta\chi^2 =
\chi^2\_{\text{constrained}} - \chi^2\_{\text{free}}\$\$ with degrees of
freedom equal to the difference in the number of free parameters. A
significant result indicates that the constraint worsens fit.

## See also

[`fit_max_sdt`](https://cgtza2.github.io/r4lineups/reference/fit_max_sdt.md)

## Examples

``` r
fit_free <- fit_max_sdt(
  n_hit=69, n_tp_choose=82, n_fa=64, N_tp=96, N_ta=106,
  n_hit_2=67, n_tp_choose_2=78, n_fa_2=42, N_tp_2=90, N_ta_2=96
)
fit_eqd <- fit_max_sdt(
  n_hit=69, n_tp_choose=82, n_fa=64, N_tp=96, N_ta=106,
  n_hit_2=67, n_tp_choose_2=78, n_fa_2=42, N_tp_2=90, N_ta_2=96,
  constrain_d = TRUE
)
compare_max_sdt(fit_free, fit_eqd)
#> MAX SDT Model Comparison (chi-squared difference test)
#>   Free model:       chi-sq = 4.659 (4 free params)
#>   Constrained model: chi-sq = 6.081 (3 free params)
#>   Constraint: d' equal
#>   Delta chi-sq(1) = 1.422, p = 0.2331
#>   Interpretation: constraint does not significantly worsen fit.
```
