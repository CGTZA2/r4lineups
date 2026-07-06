# Simulate Lineup Identification Data

Generates simulated eyewitness lineup data based on signal detection
theory parameters. Useful for power analysis, method validation, and
teaching.

## Usage

``` r
simulate_lineup_data(
  n_tp = 100,
  n_ta = 100,
  d_prime = 1.5,
  c_criterion = 0,
  lineup_size = 6,
  conf_levels = 5,
  decision_rule = c("max", "best_rest", "ensemble", "integration"),
  include_response_time = FALSE,
  seed = NULL
)
```

## Arguments

- n_tp:

  Integer. Number of target-present lineups (default = 100)

- n_ta:

  Integer. Number of target-absent lineups (default = 100)

- d_prime:

  Numeric. Discriminability (d') between target and lure distributions.
  Higher values = better memory. Typical range: 0.5 to 3.0 (default =
  1.5)

- c_criterion:

  Numeric or vector. Decision criterion/criteria for making
  identifications. Lower values = more liberal (more IDs). Can be a
  single value or vector for multiple confidence bins (default = 0)

- lineup_size:

  Integer. Number of lineup members (default = 6)

- conf_levels:

  Integer. Number of confidence levels to simulate. If NULL, returns
  binary decision only (default = 5)

- decision_rule:

  Character. Decision strategy for lineup choices:

  - "max" - Independent observations, choose highest strength (default)

  - "best_rest" - Compare best match vs average of remaining members

  - "ensemble" - Average memory strength across all lineup members

  - "integration" - Sum memory strengths across all members

- include_response_time:

  Logical. Whether to simulate response times correlated with memory
  strength (default = FALSE)

- seed:

  Integer. Random seed for reproducibility (default = NULL)

## Value

A dataframe with columns:

- participant_id: Unique ID for each trial

- target_present: Logical. TRUE if target in lineup

- identification: Character. "suspect", "filler", or "reject"

- confidence: Numeric (if conf_levels specified). Confidence rating

- response_time: Numeric (if include_response_time = TRUE). RT in ms

## Details

This function implements signal detection models with multiple decision
rules:

- Target distribution: Normal(d_prime, 1)

- Lure distribution: Normal(0, 1)

- Decision: Depends on decision_rule parameter

\*\*Decision Rules:\*\*

- \*\*MAX\*\* (default): Independent observations model. Choose lineup
  member with highest memory strength (Clark, 2003).

- \*\*BEST-REST\*\*: Compare best match against average of remaining
  members. Decision variable = best - mean(others) (Wixted et al.,
  2018).

- \*\*Ensemble\*\*: Average memory strength across lineup members using
  ensemble coding principles. More holistic evaluation (Wixted et al.,
  2018).

- \*\*Integration\*\*: Sum memory strengths across all lineup members.
  Represents complete integration of evidence (Wixted et al., 2018).

The simulation assumes:

- Fair lineups (all fillers equally similar to description)

- Perfect attention (no guessing without memory)

- Normal distributions for memory strength

Response times (if simulated):

- Based on drift-diffusion model logic

- Faster for stronger memory signals

- RT ~ InverseGaussian(strength-dependent)

## References

Wixted, J. T., Vul, E., Mickes, L., & Wilson, B. M. (2018). Models of
lineup memory. *Cognitive Psychology, 105*, 8-114.

Mickes, L., et al. (2024). pyWitness 1.0: A python eyewitness
identification analysis toolkit. *Behavior Research Methods, 56*,
1533-1550.

## Examples

``` r
# Basic simulation: strong memory with MAX rule (default)
strong_memory <- simulate_lineup_data(
  n_tp = 200, n_ta = 200,
  d_prime = 2.0,
  conf_levels = 3,
  seed = 42
)

# Ensemble decision rule
ensemble_data <- simulate_lineup_data(
  n_tp = 200, n_ta = 200,
  d_prime = 1.5,
  decision_rule = "ensemble",
  conf_levels = 5,
  seed = 42
)

# Integration model with response times
integration_data <- simulate_lineup_data(
  n_tp = 200, n_ta = 200,
  d_prime = 1.5,
  decision_rule = "integration",
  conf_levels = 5,
  include_response_time = TRUE,
  seed = 42
)

# Compare ROC curves across decision rules
roc_max <- make_roc(strong_memory, lineup_size = 6)
roc_ensemble <- make_roc(ensemble_data, lineup_size = 6)

# \donttest{
# Power analysis: vary sample size
power_results <- simulate_power_analysis(
  sample_sizes = c(50, 100),
  d_prime = 1.5,
  n_simulations = 100
)
#> Simulating sample size: 50 
#> Simulating sample size: 100 
# }
```
