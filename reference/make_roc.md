# Compute and Plot ROC Curve for Lineup Identification

Main function to compute and plot an ROC curve for eyewitness lineup
data. This follows the methodology of Wixted & Mickes (2012) and Mickes
(2015).

## Usage

``` r
make_roc(
  data,
  lineup_size = 6,
  show_plot = TRUE,
  innocent_suspect = "auto",
  ...
)
```

## Arguments

- data:

  A dataframe with the following columns:

  - target_present: Logical. TRUE if guilty suspect in lineup

  - identification: Character. "suspect", "filler", or "reject"

  - confidence: Numeric. Confidence rating

- lineup_size:

  Integer. Number of people in lineup (default = 6)

- show_plot:

  Logical. Whether to display the plot (default = TRUE)

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

- ...:

  Additional arguments passed to make_roc_gg()

## Value

A list containing:

- plot: ggplot2 object. Always returned, whatever show_plot is set to;
  show_plot controls only whether it is displayed immediately

- roc_data: Dataframe with ROC curve points

- pauc: Partial area under the curve

- summary: Summary statistics

## Details

ROC analysis measures discriminability - the ability to distinguish
innocent from guilty suspects. According to Mickes (2015), ROC analysis
is most relevant for policymakers deciding on system variables (e.g.,
simultaneous vs. sequential lineups, lineup size, etc.).

For estimator variables (e.g., exposure duration, retention interval),
use CAC analysis instead (see
[`make_cac`](https://cgtza2.github.io/r4lineups/reference/make_cac.md)).

## References

Wixted, J. T., & Mickes, L. (2012). The field of eyewitness memory
should abandon probative value and embrace receiver operating
characteristic analysis. *Perspectives on Psychological Science, 7*(3),
275-278.

Mickes, L. (2015). Receiver operating characteristic analysis and
confidence-accuracy characteristic analysis in investigations of system
variables and estimator variables that affect eyewitness memory.
*Journal of Applied Research in Memory and Cognition, 4*(2), 93-102.

## Examples

``` r
data(lineup_example)
roc_result <- make_roc(lineup_example)

print(roc_result$pauc)
#> [1] 0.05333776
```
