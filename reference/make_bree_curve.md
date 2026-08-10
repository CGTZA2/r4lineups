# Compute Base-Rate Effect-Equivalency (BREE) Curves

Computes BREE curves showing the base-rate shift required for one
procedure to produce the same posterior probability as another
procedure, following Wells, Yang, & Smalarz (2015).

## Usage

``` r
make_bree_curve(
  data_proc_a,
  data_proc_b,
  reference_response = "suspect",
  confidence_bins = NULL,
  prior_grid = seq(0.01, 0.99, 0.01),
  lineup_size = 6
)
```

## Arguments

- data_proc_a:

  A dataframe for procedure A with columns: target_present,
  identification, confidence (standard lineup data format)

- data_proc_b:

  A dataframe for procedure B (same format as proc_a)

- reference_response:

  Character string specifying which response to compare (e.g., "suspect"
  for suspect IDs). Default = "suspect".

- confidence_bins:

  Optional numeric vector of bin edges if using confidence-based
  response (e.g., c(0, 60, 80, 100)). If provided, reference_response
  should specify both identification and bin (e.g.,
  "suspect\_(80,100\]")

- prior_grid:

  Numeric vector of prior probabilities for procedure A (default:
  seq(0.01, 0.99, 0.01))

- lineup_size:

  Nominal lineup size for procedures without a designated innocent
  suspect.

## Value

A list containing:

- bree_curve: Dataframe with prior_a, posterior_a, prior_b, and delta

- proc_a_likelihoods: Likelihoods for procedure A

- proc_b_likelihoods: Likelihoods for procedure B

- reference_response: The response category used for comparison

## Details

BREE curves answer the question: "How much would the base rate need to
change for Procedure B to yield the same posterior probability as
Procedure A?"

For each prior probability in procedure A: 1. Compute the posterior
p(guilty\|x) for the reference response in procedure A 2. Find the prior
probability in procedure B that yields the same posterior 3. Delta =
prior_B - prior_A

Interpretation:

- Delta \> 0: Procedure B requires a higher base rate to match A's
  posterior (A is more diagnostic)

- Delta \< 0: Procedure B requires a lower base rate (B is more
  diagnostic)

- Delta = 0: Procedures are equally diagnostic

## References

Wells, G. L., Yang, Y., & Smalarz, L. (2015). Eyewitness identification:
Bayesian information gain, base-rate effect-equivalency curves, and
reasonable suspicion. *Law and Human Behavior, 39*(2), 99-122.

## Examples

``` r
data(lineup_example)
odd <- seq(1, nrow(lineup_example), by = 2)
bree <- make_bree_curve(lineup_example[odd, ], lineup_example[-odd, ])
head(bree$bree_curve)
#> # A tibble: 6 × 4
#>   prior_a posterior_a prior_b   delta
#>     <dbl>       <dbl>   <dbl>   <dbl>
#> 1    0.01      0.0571  0.0198 0.00980
#> 2    0.02      0.109   0.0392 0.0192 
#> 3    0.03      0.157   0.0583 0.0283 
#> 4    0.04      0.2     0.0769 0.0369 
#> 5    0.05      0.24    0.0952 0.0452 
#> 6    0.06      0.277   0.113  0.0532 
```
