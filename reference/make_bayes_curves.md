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
  prior_grid = seq(0.01, 0.99, 0.01),
  lineup_size = 6,
  innocent_suspect = "auto",
  gain = c("probability", "signed", "entropy")
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

- lineup_size:

  Nominal lineup size, used when target-absent data have no designated
  innocent suspect.

- innocent_suspect:

  How innocent-suspect identifications in target-absent lineups are
  obtained. `"designated"`: the lineups had a designated innocent
  suspect, so target-absent `"suspect"` rows are counted directly, and
  zero such rows means a false-ID rate of zero. `"estimated"`: there was
  no designated innocent suspect, so the innocent-suspect count is
  estimated as filler IDs divided by `lineup_size`. `"auto"` (the
  default) chooses `"designated"` when any target-absent row is a
  suspect ID or the data have a non-missing `suspect_pos` column, and
  `"estimated"` otherwise, with a message.

- gain:

  Character. How information gain is measured. `"probability"` (default)
  is the measure of Wells and Lindsay (1980) and Wells, Yang and Smalarz
  (2015): the absolute difference between posterior and prior,
  \|posterior - prior\|, on the probability scale. `"signed"` is the
  same difference with its sign kept (positive when the response raises
  the probability of guilt, negative when it lowers it), as in Smith,
  Lindsay and Wells (2016). `"entropy"` is the reduction in binary
  entropy, H(prior) minus H(posterior), in bits (Starns, Cohen &
  Rotello, 2023); it is the per-response term of expected information
  gain
  ([`compute_eig`](https://cgtza2.github.io/r4lineups/reference/compute_eig.md)).

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

**Information gain** (`gain = "probability"`; Wells & Lindsay, 1980;
Wells et al., 2015): \$\$IG(x) = \|p(guilty\|x) - p(guilty)\|\$\$ Wells
and Lindsay use the absolute value because the amount of change, not its
direction, measures the information gained. `gain = "signed"` returns
\\p(guilty\|x) - p(guilty)\\, positive for responses that incriminate
the suspect and negative for responses that exonerate; Smith et al.
(2016) report gain this way.

**Entropy reduction** (`gain = "entropy"`, Starns et al., 2023):
\$\$IG(x) = H(prior) - H(posterior)\$\$ where H(p) is binary entropy in
bits. It is negative when the response moves belief towards 0.5, as a
suspect identification does at a low prior; that is a correct update
that leaves more uncertainty, not misleading evidence. The two measures
can differ in sign, so state which one is reported.

## References

Wells, G. L., & Lindsay, R. C. L. (1980). On estimating the
diagnosticity of eyewitness nonidentifications. *Psychological Bulletin,
88*(3), 776-784.

Wells, G. L., Yang, Y., & Smalarz, L. (2015). Eyewitness identification:
Bayesian information gain, base-rate effect-equivalency curves, and
reasonable suspicion. *Law and Human Behavior, 39*(2), 99-122.

Smith, A. M., Lindsay, R. C. L., & Wells, G. L. (2016). A Bayesian
analysis on the (dis)utility of iterative-showup procedures: The
moderating impact of prior probabilities. *Law and Human Behavior,
40*(5), 503-516.
[doi:10.1037/lhb0000196](https://doi.org/10.1037/lhb0000196)

Starns, J. J., Cohen, A. L., & Rotello, C. M. (2023). A complete method
for assessing the effectiveness of eyewitness identification procedures:
Expected information gain. *Psychological Review, 130*(3), 677-719.

## Examples

``` r
data(lineup_example)
bayes <- make_bayes_curves(lineup_example)
bayes$likelihoods
#> # A tibble: 3 × 5
#>   response p_x_given_guilty p_x_given_innocent n_guilty n_innocent
#>   <chr>               <dbl>              <dbl>    <int>      <dbl>
#> 1 filler             0.311               0.587      218        411
#> 2 reject             0.0957              0.287       67        201
#> 3 suspect            0.593               0.126      415         88
```
