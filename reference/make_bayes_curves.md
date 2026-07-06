# Compute Bayesian Prior-Posterior and Information Gain Curves

Computes Bayesian updating curves showing how different lineup responses
update beliefs about suspect guilt, following Wells, Yang, & Smalarz
(2015).

## Usage

``` r
make_bayes_curves(
  data,
  response_categories = c("simple", "confidence"),
  confidence_bins = NULL,
  prior_grid = seq(0.01, 0.99, 0.01)
)
```

## Arguments

- data:

  A dataframe with the following columns:

  - target_present: Logical. TRUE if guilty suspect in lineup

  - identification: Character. "suspect", "filler", or "reject"

  - confidence: Numeric. Confidence rating (optional for response
    categories)

- response_categories:

  Character vector specifying how to categorize responses. Options:

  - "simple": Three categories (suspect, filler, reject)

  - "confidence": Combines identification with confidence bins

- confidence_bins:

  Numeric vector of bin edges if using confidence categories (e.g., c(0,
  60, 80, 100)). Ignored if response_categories = "simple".

- prior_grid:

  Numeric vector of prior probabilities to evaluate (default: seq(0.01,
  0.99, 0.01))

## Value

A list containing:

- curves: Dataframe with prior, posterior, and information gain for each
  response

- likelihoods: Dataframe with p(x\|guilty) and p(x\|innocent) for each
  response

- response_counts: Count of each response type by target presence

- n_guilty: Number of target-present lineups

- n_innocent: Number of target-absent lineups

## Details

This function treats lineup outcomes as evidence for Bayesian updating.
For each response type x (e.g., high-confidence suspect ID, rejection,
etc.), it computes:

**Likelihoods from observed data:** \$\$p(x\|guilty) = \frac{count(x \|
target\\present)}{N\_{guilty}}\$\$ \$\$p(x\|innocent) = \frac{count(x \|
target\\absent)}{N\_{innocent}}\$\$

**For each prior probability p(guilty):** \$\$p(x) = p(guilty) \cdot
p(x\|guilty) + (1-p(guilty)) \cdot p(x\|innocent)\$\$ \$\$p(guilty\|x) =
\frac{p(guilty) \cdot p(x\|guilty)}{p(x)}\$\$

**Information gain:** \$\$IG(x) = H(prior) - H(posterior)\$\$

where H(p) is binary entropy.

The information gain quantifies how much uncertainty is reduced by
observing response x. Positive values indicate the response is
diagnostic.

## References

Wells, G. L., Yang, Y., & Smalarz, L. (2015). Eyewitness identification:
Bayesian information gain, base-rate effect-equivalency curves, and
reasonable suspicion. *Law and Human Behavior, 39*(2), 99-122.

## Examples

``` r
data(lineup_example)
bayes <- make_bayes_curves(lineup_example)
bayes$likelihoods
#> # A tibble: 3 × 5
#>   response p_x_given_guilty p_x_given_innocent n_guilty n_innocent
#>   <chr>               <dbl>              <dbl>    <int>      <int>
#> 1 suspect              0.6                0.15       60         15
#> 2 reject               0.22               0.61       22         61
#> 3 filler               0.18               0.24       18         24
```
