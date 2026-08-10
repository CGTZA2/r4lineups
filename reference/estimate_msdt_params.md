# mSDT Parameter Estimation from Rejection Rates

Estimate decision criterion (gamma) and discriminability (d') from
rejection rates in culprit-absent and culprit-present lineups.

## Usage

``` r
estimate_msdt_params(pr_rej_I, pr_rej_G, lineup_size, eps = 1e-06)

msdt_gamma_from_rej(pr_rej_I, lineup_size, eps = 1e-06)

msdt_dprime_from_rej(pr_rej_G, gamma, lineup_size, eps = 1e-06)
```

## Arguments

- pr_rej_I:

  Rejection rate in culprit-absent lineups.

- pr_rej_G:

  Rejection rate in culprit-present lineups.

- lineup_size:

  Lineup size (suspect + fillers), must be \>= 2.

- eps:

  Small value used to avoid probabilities of 0 or 1.

- gamma:

  Decision criterion estimated from culprit-absent rejection rates,
  typically returned by `msdt_gamma_from_rej()`.

## Value

A list with gamma and dprime.

## Details

This is a **method-of-moments estimator** that is exactly identified
from the two rejection rates alone: Pr(REJ\|I) fixes gamma, and the gap
between culprit-absent and culprit-present rejection rates fixes d'. It
is **information-lossy**: it ignores the composition of choices
(suspect-ID vs filler-ID) and the spread of filler choices, which are
the most diagnostic outcomes. If you have the full response breakdown
(suspect/filler/reject counts for both lineup types), prefer the
minimum-Pearson fitter
[`fit_max_sdt`](https://cgtza2.github.io/r4lineups/reference/fit_max_sdt.md)
or the 2-HT model
([`fit_winter_2ht`](https://cgtza2.github.io/r4lineups/reference/fit_winter_2ht.md)),
which use all outcome counts.

## See also

[`fit_max_sdt`](https://cgtza2.github.io/r4lineups/reference/fit_max_sdt.md)
for a full-outcome, minimum-Pearson fit;
[`fit_winter_2ht`](https://cgtza2.github.io/r4lineups/reference/fit_winter_2ht.md)
for the 2-HT MPT model.

## Examples

``` r
estimate_msdt_params(pr_rej_I = 0.25, pr_rej_G = 0.15, lineup_size = 6)
#> $gamma
#> [1] 0.8193286
#> 
#> $dprime
#> [1] 0.8789708
#> 
```
