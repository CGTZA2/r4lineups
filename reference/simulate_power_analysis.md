# Simulate Power Analysis for Lineup Studies

Conducts power analysis by simulating data across different sample sizes
and computing a statistic of interest (e.g., pAUC, d').

## Usage

``` r
simulate_power_analysis(
  sample_sizes,
  d_prime,
  n_simulations = 1000,
  stat_function = NULL,
  alpha = 0.05,
  ...
)
```

## Arguments

- sample_sizes:

  Integer vector. Sample sizes to test (e.g., c(50, 100, 200))

- d_prime:

  Numeric. True effect size (d') to detect

- n_simulations:

  Integer. Number of simulations per sample size (default = 1000)

- stat_function:

  Function. Takes a dataframe and returns a statistic. Default computes
  pAUC from ROC analysis.

- alpha:

  Numeric. Significance level (default = 0.05)

- ...:

  Additional arguments passed to simulate_lineup_data()

## Value

A dataframe with columns:

- sample_size: The N tested

- mean_stat: Mean value of statistic

- sd_stat: Standard deviation of statistic

- ci_lower: Lower 95

- ci_upper: Upper 95

- power: Proportion of simulations where effect was detected

## Examples

``` r
# \donttest{
# Power to detect d' = 1.5 with pAUC
power_res <- simulate_power_analysis(
  sample_sizes = c(50, 100),
  d_prime = 1.5,
  n_simulations = 100,
  conf_levels = 5,
  seed = 123
)
#> Simulating sample size: 50 
#> Simulating sample size: 100 
print(power_res)
#>       sample_size mean_stat sd_stat ci_lower ci_upper power
#> 2.5%           50   0.08340       0  0.08340  0.08340     1
#> 2.5%1         100   0.08035       0  0.08035  0.08035     1
# }
```
