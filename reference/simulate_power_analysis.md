# Simulate Power Analysis for Lineup Studies

Estimates statistical power to detect a difference in discriminability
between two lineup conditions, by simulating both conditions at a range
of sample sizes and recording how often the difference is detected.

## Usage

``` r
simulate_power_analysis(
  sample_sizes,
  d_prime,
  d_prime_2 = 0,
  n_simulations = 1000,
  test_function = NULL,
  alpha = 0.05,
  lineup_size = 6,
  n_bootstrap = 200,
  seed = NULL,
  verbose = TRUE,
  d_prime_null = NULL,
  ...
)
```

## Arguments

- sample_sizes:

  Integer vector of sizes to test (e.g., c(50, 100, 200)). A size n
  means n target-present *and* n target-absent witnesses in each of the
  two simulated conditions: 2n witnesses per condition and 4n in the
  whole simulated experiment.

- d_prime:

  Numeric. Discriminability of the condition of interest.

- d_prime_2:

  Numeric. Generating discriminability of the second (comparison)
  condition. It is not the null hypothesis: the test asks whether the
  two conditions differ. The default, 0, is a condition with no memory
  of the culprit, so it asks how often the procedure can be shown to
  discriminate better than chance. For a comparison of two working
  procedures, set it to a realistic value (for example `d_prime = 1.5`,
  `d_prime_2 = 1.2`). Equal values estimate the test's rejection rate
  under the null.

- n_simulations:

  Integer. Number of simulated studies per sample size (default = 1000).
  Power is a proportion estimated from these, so its own standard error
  is roughly `sqrt(0.25 / n_simulations)`.

- test_function:

  Function of two dataframes returning a p-value. Defaults to a
  bootstrap comparison of partial AUC via
  [`compare_pauc`](https://cgtza2.github.io/r4lineups/reference/compare_pauc.md).

- alpha:

  Numeric. Significance level (default = 0.05)

- lineup_size:

  Integer. Lineup size for both conditions (default = 6)

- n_bootstrap:

  Integer. Bootstrap resamples used by the default test (default = 200).
  This is the dominant cost: the default test is run `n_simulations`
  times for every sample size.

- seed:

  Optional integer seed for reproducibility.

- verbose:

  Logical. Report progress (default = TRUE).

- d_prime_null:

  Deprecated name for `d_prime_2`.

- ...:

  Additional arguments passed to simulate_lineup_data(), for example
  `c_criterion`, `conf_levels` or `foil_bias`. Set `c_criterion` to
  match the rejection rate you expect: the default, 0, makes witnesses
  choose almost always under the MAX rule (see
  [`simulate_lineup_data`](https://cgtza2.github.io/r4lineups/reference/simulate_lineup_data.md)),
  and the power then applies to that setting.

## Value

A dataframe of class `power_analysis` with columns:

- sample_size: n, the number of target-present (and, separately,
  target-absent) witnesses per condition

- n_total: Witnesses in the whole simulated experiment, 4n

- power: Proportion of simulated studies reaching significance

- power_se: Standard error of that proportion

- mean_effect: Mean observed difference in pAUC

- sd_effect: Standard deviation of the observed difference

- n_valid: Simulations that produced a usable test

- n_failed: Attempted simulations without a usable test

- n_attempted: All attempted simulations

Power and its Monte Carlo standard error are conditional on usable
tests. Failures are retained in the attempted count and trigger a
warning; compare failure rates as well as conditional power when
assessing a design.

## Details

Power is the probability that a study of a given size detects an effect
that is really there. Estimating it by simulation means specifying the
effect, generating many studies of that size, applying the analysis you
intend to use, and counting how often it reaches significance.

That requires a second condition to compare against, which `d_prime_2`
supplies; the null hypothesis is that the two conditions do not differ.
Simulating a single condition and computing a statistic from it is not a
power analysis, because no hypothesis is being tested.

Each simulated study samples witnesses for one lineup set (one fixed
foil structure) and gives every witness the same \\d'\\ and criterion.
The power is therefore for generalising over witnesses to these
materials. Generalising over culprits and lineups as well needs sets
sampled as units, and with few sets the effective sample size for that
question is closer to the number of sets (Wells & Windschitl, 1999;
Westfall, Kenny, & Judd, 2014).

Power is estimated from `n_simulations` studies, so it has Monte Carlo
error, and each default test rests on `n_bootstrap` resamples. An
estimated power of 1 (standard error 0) means only that the true power
is high: by the rule of three, with 30 simulations it is above about
\\1 - 3/30 = 0.90\\ with 95% confidence.

## References

Wells, G. L., & Windschitl, P. D. (1999). Stimulus sampling and social
psychological experimentation. *Personality and Social Psychology
Bulletin, 25*, 1115–1125.

Westfall, J., Kenny, D. A., & Judd, C. M. (2014). Statistical power and
optimal design in experiments in which samples of participants respond
to samples of stimuli. *Journal of Experimental Psychology: General,
143*, 2020–2045.

## See also

[`compare_pauc`](https://cgtza2.github.io/r4lineups/reference/compare_pauc.md),
[`simulate_lineup_data`](https://cgtza2.github.io/r4lineups/reference/simulate_lineup_data.md)

## Examples

``` r
# \donttest{
# Power to show discriminability better than chance
power_res <- simulate_power_analysis(
  sample_sizes = c(50, 100),
  d_prime = 1.5,
  c_criterion = 1.23,
  n_simulations = 8,
  n_bootstrap = 50,
  conf_levels = 5,
  seed = 123
)
#> Simulating sample size 50 (8 studies)
#> Simulating sample size 100 (8 studies)
print(power_res)
#>   sample_size n_total power  power_se mean_effect   sd_effect n_valid n_failed
#> 1          50     200  0.75 0.1530931  0.02025417 0.012625132       8        0
#> 2         100     400  0.75 0.1530931  0.01616521 0.009858588       8        0
#>   n_attempted
#> 1           8
#> 2           8
# }
```
