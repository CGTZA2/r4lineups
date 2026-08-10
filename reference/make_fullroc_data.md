# Compute Full ROC Data for Lineup Identification (Smith & Yang, 2020)

Computes full Receiver Operating Characteristic (ROC) data using ALL
response categories (suspect ID, filler ID, rejection) following Smith &
Yang (2020). This method distinguishes between investigator
discriminability and eyewitness discriminability.

## Usage

``` r
make_fullroc_data(
  data,
  conf_bins = NULL,
  order = c("diagnosticity", "apriori"),
  lineup_size = 6,
  epsilon = 0.001
)
```

## Arguments

- data:

  A dataframe with the following columns:

  - target_present: Logical. TRUE if guilty suspect in lineup, FALSE if
    innocent

  - identification: Character. "suspect", "filler", or "reject"

  - confidence: Numeric. Confidence rating (higher = more confident)

- conf_bins:

  Numeric vector specifying confidence bin boundaries (default = NULL,
  which uses unique confidence values). Can specify bins like c(0, 60,
  80, 100) to create low (0-60), medium (60-80), and high (80-100)
  confidence categories.

- order:

  Character. Method for ordering response categories:

  - "diagnosticity": Order by diagnosticity ratio (DR = HR/FAR)
    (default)

  - "apriori": Use a-priori ordering (suspect high conf → filler/reject
    low conf)

- lineup_size:

  Integer. Number of people in the lineup (default = 6)

- epsilon:

  Numeric. Small value to add when FAR = 0 to avoid infinite DR (default
  = 0.001)

## Value

A list containing:

- roc_data: Dataframe with cumulative hit rates and false alarm rates

- auc: Full area under the curve

- order_used: The ordering method used

- diagnosticity_table: Table showing non-cumulative rates and DR for
  each category

- n_target_present: Number of target-present lineups

- n_target_absent: Number of target-absent lineups

## Details

This function implements the "full ROC" method described in Smith & Yang
(2020). Unlike traditional partial ROC curves that only use suspect
identifications, this method uses ALL eyewitness responses:

- Suspect identifications (evidence of guilt)

- Filler identifications (evidence of innocence)

- Lineup rejections (evidence of innocence)

Each response type is crossed with confidence levels to create decision
criteria. These are ordered by their diagnosticity ratio (HR/FAR) or by
a-priori ordering, then cumulative hit and false alarm rates are
computed to form the full ROC curve.

The full ROC provides a threshold-free measure of investigator
discriminability— the ability to distinguish guilty from innocent
suspects using ALL available eyewitness evidence.

**Note on ordering and bias:** ordering cells by the *sample*
diagnosticity ratio (`order = "diagnosticity"`) bows the curve upward
from noise alone, so the resulting AUC is optimistically biased —
identical target-present and target-absent response distributions yield
AUC slightly above 0.5, with the bias growing with the number of
decision-by-confidence cells and shrinking with sample size. For
inference or comparisons across conditions, prefer `order = "apriori"`
(the theoretically fixed ordering suggested by Smith & Yang, 2020) or
compare against a permutation baseline.

## References

Smith, A. M., Yang, Y., & Wells, G. L. (2020). Distinguishing between
investigator discriminability and eyewitness discriminability: A method
for creating full receiver operating characteristic curves of lineup
identification performance. *Perspectives on Psychological Science,
15*(3), 589-607.

## Examples

``` r
data(lineup_example)
# Compute full ROC with diagnosticity ordering
fullroc_result <- make_fullroc_data(lineup_example)
print(fullroc_result$auc)
#> [1] 0.8589

# Compute full ROC with specified confidence bins
fullroc_binned <- make_fullroc_data(lineup_example, conf_bins = c(0, 60, 80, 100))
```
