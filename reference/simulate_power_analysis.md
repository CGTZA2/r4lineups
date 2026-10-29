# Simulate Power Analysis for Lineup Studies

Estimates statistical power to detect a difference in discriminability
between two lineup conditions, by simulating both conditions at a range
of sample sizes and recording how often the difference is detected.

## Usage

``` r
simulate_power_analysis(
  sample_sizes,
  d_prime,
  d_prime_null = 0,
  n_simulations = 1000,
  test_function = NULL,
  alpha = 0.05,
  lineup_size = 6,
  n_bootstrap = 200,
  seed = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- sample_sizes:

  Integer vector. Sample sizes per condition to test (e.g., c(50, 100,
  200)). Each is used for both target-present and target-absent lineups
  in each condition.

- d_prime:

  Numeric. Discriminability of the condition of interest.

- d_prime_null:

  Numeric. Discriminability of the comparison condition. Defaults to 0,
  which asks how often the procedure can be shown to discriminate better
  than chance. Set it to a non-zero value to ask the more usual question
  of how often two working procedures can be told apart.

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

- ...:

  Additional arguments passed to simulate_lineup_data(), for example
  `conf_levels` or `foil_bias`.

## Value

A dataframe of class `power_analysis` with columns:

- sample_size: N per condition

- power: Proportion of simulated studies reaching significance

- power_se: Standard error of that proportion

- mean_effect: Mean observed difference in pAUC

- sd_effect: Standard deviation of the observed difference

- n_valid: Simulations that produced a usable test

## Details

Power is the probability that a study of a given size detects an effect
that is really there. Estimating it by simulation means specifying the
effect, generating many studies of that size, applying the analysis you
intend to use, and counting how often it reaches significance.

That requires a null to test against, which is what `d_prime_null`
supplies. Simulating a single condition and computing a statistic from
it is not a power analysis, because no hypothesis is being tested.

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
  n_simulations = 8,
  n_bootstrap = 50,
  conf_levels = 5,
  seed = 123
)
#> Simulating sample size 50 (8 studies)
#> Simulating sample size 100 (8 studies)
print(power_res)
#>   sample_size power  power_se mean_effect  sd_effect n_valid
#> 1          50 0.875 0.1169268  0.04444208 0.01567793       8
#> 2         100 1.000 0.0000000  0.04280260 0.01922859       8
# }
```
