# Compute PPV by Confidence with Lineup Size Correction

Calculates Positive Predictive Value (PPV) for each confidence level,
using a specified method to correct for lineup size when estimating
innocent-suspect identification rates.

## Usage

``` r
ppv_by_confidence(
  data,
  lineup_size = 6,
  confidence_bins = NULL,
  correction = c("nominal", "effective", "none"),
  effective_size_data = NULL,
  quiet = FALSE
)
```

## Arguments

- data:

  A dataframe with columns:

  - target_present: Logical. TRUE if guilty suspect in lineup

  - identification: Character. "suspect", "filler", or "reject"

  - confidence: Numeric. Confidence rating

- lineup_size:

  Integer. Nominal number of people in lineup (default = 6)

- confidence_bins:

  Numeric vector of bin edges for grouping confidence (e.g., c(0, 60,
  80, 100)). If NULL, uses individual confidence levels.

- correction:

  Character. Method for estimating innocent-suspect IDs: "nominal"
  (best-case, assumes fair lineup), "effective" (accounts for bias),
  "none" (worst-case, no correction). Default = "nominal".

- effective_size_data:

  Optional dataframe with pre-computed effective sizes per confidence
  bin (must have columns: conf_level, effective_size). If NULL and
  correction="effective", computes from data.

- quiet:

  Logical. Suppress the note issued when `correction` is not applied
  because the data designate an innocent suspect. Default `FALSE`.

## Value

A list containing:

- ppv_data: Dataframe with PPV by confidence level

- overall_ppv: Overall PPV across all confidence levels

- correction_method: The correction method requested

- innocent_suspect_method: "designated" when target-absent trials
  identify an innocent suspect directly, otherwise the estimation route

- lineup_size: Nominal lineup size

## Details

PPV (Positive Predictive Value) is the probability that a suspect is
guilty given an identification. The formula is:

PPV = guilty_suspect_IDs / (guilty_suspect_IDs + innocent_suspect_IDs)

Different correction methods estimate innocent_suspect_IDs differently:

- "nominal": assumes perfect lineup fairness (error_rate / lineup_size)

- "effective": uses effective size to account for bias (error_rate /
  effective_size)

- "none": assumes all errors are innocent-suspect IDs (error_rate)

**Note on the implicit base rate:** PPV is computed from raw
target-present and target-absent counts, so the prior probability of
guilt is implicitly the experiment's TP:TA ratio (e.g., 0.5 for the
usual balanced design). PPV is *not* base-rate-free: at different
real-world base rates the PPV of the same procedure will differ. When
target-absent lineups have no designated innocent suspect, the
correction also relies on the pseudo-distribution assumption of
spreading filler choices over lineup members, which is a modeling
approximation.

## When the correction is ignored

Lineup-size corrections exist to *estimate* how many of the observed
target-absent identifications fell on an innocent suspect, in data where
no innocent suspect was designated. When the data do designate one, that
count is observed rather than estimated, and `correction` has no work to
do: all three methods return the same PPV. This is correct, not a fault,
but it is easy to mistake for one, so a message is issued unless
`quiet = TRUE`. `lineup_example` is data of this kind.

## References

Fitzgerald, R. J., Tredoux, C. G., & Juncu, S. (2023). Estimation of
eyewitness error rates in fair and biased lineups. *Law and Human
Behavior*.

## Examples

``` r
data(lineup_example)
# Nominal correction (assumes fair lineup)
ppv_result <- ppv_by_confidence(lineup_example,
                                correction = "nominal",
                                confidence_bins = c(0, 60, 80, 100))
#> Target-absent lineups contain a designated innocent suspect, so innocent-suspect IDs are counted directly and the 'nominal' correction is not applied. All correction methods return the same PPV for these data. Lineup-size corrections apply only when no innocent suspect is designated.

# Effective size correction (accounts for bias)
ppv_result <- ppv_by_confidence(lineup_example,
                                correction = "effective",
                                confidence_bins = c(0, 60, 80, 100))
#> Target-absent lineups contain a designated innocent suspect, so innocent-suspect IDs are counted directly and the 'effective' correction is not applied. All correction methods return the same PPV for these data. Lineup-size corrections apply only when no innocent suspect is designated.
```
