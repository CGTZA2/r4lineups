# Compute Expected Utility Curves for Lineup Identification

Computes expected utility at different confidence criteria following
Lampinen, Smith, & Wells (2019). Utility analysis accounts for the costs
and benefits of different identification outcomes.

## Usage

``` r
make_utility_curves(
  data,
  base_rate = 0.5,
  utility_matrix = c(tp = 1, fn = -0.5, fp = -2, tn = 0.5),
  lineup_size = 6,
  criteria = c("confidence", "all")
)
```

## Arguments

- data:

  A dataframe with the following columns:

  - target_present: Logical. TRUE if guilty suspect in lineup

  - identification: Character. "suspect", "filler", or "reject"

  - confidence: Numeric. Confidence rating (higher = more confident)

- base_rate:

  Numeric. Prior probability that suspect is guilty (0 to 1). Default =
  0.5.

- utility_matrix:

  Named numeric vector with utilities/costs for:

  - tp: True positive (correct suspect ID) - typically positive

  - fn: False negative (miss/rejection of guilty) - typically negative

  - fp: False positive (innocent suspect ID) - typically very negative

  - tn: True negative (correct rejection of innocent) - typically
    positive

  Default: c(tp=1, fn=-0.5, fp=-2, tn=0.5)

- lineup_size:

  Integer. Number of people in lineup (default = 6). Used to estimate
  false IDs from filler choices.

- criteria:

  Character. How to define decision criteria:

  - "confidence": Use confidence thresholds (cumulative from high to
    low)

  - "all": Include all identifications regardless of confidence

  Default = "confidence".

## Value

A list containing:

- utility_data: Dataframe with criterion, hit rate, false alarm rate,
  and expected utility

- max_utility: Maximum expected utility and corresponding criterion

- avg_utility: Average expected utility across all criteria

- utility_all_ids: Expected utility if all IDs are accepted

- base_rate: Base rate used

- utility_matrix: Utility matrix used

## Details

Expected utility combines hit rates and false alarm rates with the
costs/benefits of different outcomes:

\$\$EU = base\\rate \cdot \[hit \cdot U\_{TP} + (1-hit) \cdot U\_{FN}\]
+\$\$ \$\$ (1-base\\rate) \cdot \[fa \cdot U\_{FP} + (1-fa) \cdot
U\_{TN}\]\$\$

Where:

- hit = p(suspect ID \| guilty) at criterion

- fa = p(suspect ID \| innocent) at criterion

- U_TP, U_FN, U_FP, U_TN = utilities for each outcome

The utility matrix should reflect the relative value/cost of outcomes.
For example:

- tp = 1: Correctly identifying the perpetrator

- fn = -0.5: Missing the perpetrator (they remain at large)

- fp = -2: Wrongly convicting an innocent person (severe injustice)

- tn = 0.5: Correctly rejecting when innocent

Lampinen et al. (2019) show that comparing procedures by ROC curves
alone can be misleading. Utility analysis incorporates base rates and
the relative costs of errors, providing a more complete evaluation.

**Note on the false-alarm estimate:** explicit target-absent suspect IDs
are used when present. Otherwise, filler IDs are divided by lineup size.
Direct and filler-derived estimates are never added together.

## References

Lampinen, J. M., Smith, A. M., & Wells, G. L. (2019). Four utilities in
eyewitness identification practice: Dissociations between receiver
operating characteristic analysis and expected utility analysis. *Law
and Human Behavior, 43*(1), 26-44.

## Examples

``` r
data(lineup_example)
util <- make_utility_curves(lineup_example)
util$max_utility
#> $expected_utility
#>     tp 
#> 0.3125 
#> 
#> $criterion
#> [1] 60
#> 
#> $hit_rate
#> [1] 0.6
#> 
#> $false_alarm_rate
#> [1] 0.11
#> 
```
