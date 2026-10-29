# Compute CAC (Confidence-Accuracy Characteristic) Data

Computes confidence-accuracy data for lineup identifications following
Mickes (2015). CAC analysis shows the relationship between confidence
and accuracy for suspect identifications.

## Usage

``` r
make_cacdata(
  data,
  lineup_size = 6,
  confidence_bins = NULL,
  innocent_suspect = "auto",
  base_rate = NULL
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

- confidence_bins:

  Numeric vector of bin edges for grouping confidence (e.g., c(0, 60,
  80, 100) creates bins 0-60, 61-80, 81-100). If NULL, uses individual
  confidence levels.

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

- base_rate:

  Prior probability that the suspect is guilty, used to compute accuracy
  from rates rather than counts. `NULL` (default) keeps the count
  definition of Mickes (2015). A number strictly between 0 and 1 gives
  \\\pi HR(c) / (\pi HR(c) + (1 - \pi) FAR(c))\\; see Details.

## Value

A list containing:

- cac_data: Dataframe with confidence levels and accuracy

- overall_accuracy: Overall accuracy across all suspect IDs

- n_total_suspect_ids: Correct plus incorrect suspect IDs

- base_rate: The base rate used, or NULL for count-based accuracy

- n_target_present, n_target_absent: Sample sizes of the two arms

## Details

CAC analysis computes, for each confidence level: \$\$Accuracy =
\frac{Correct Suspect IDs}{Correct Suspect IDs + Incorrect Suspect
IDs}\$\$

Explicit target-absent suspect IDs are used when present. Otherwise,
target-absent filler IDs are divided by lineup size to estimate
incorrect suspect IDs. The two estimators are never added together.

The count ratio equals the probability of guilt given a suspect ID only
when the target-present and target-absent samples are the same size,
which Mickes (2015) assumes; she notes that unequal base rates make the
calculation more complicated. With unequal samples the count-based
accuracy shifts with the TP:TA split even when hit and false-ID rates
are unchanged, and the function says so in a message. Setting
`base_rate` computes accuracy from the hit rate HR(c) = correct IDs /
n(TP) and the false-ID rate FAR(c) = incorrect IDs / n(TA) at the stated
prior, so that `base_rate = 0.5` gives the balanced-design value
whatever the sample sizes. Its standard error is then a delta-method
approximation from the two independent binomial rates; the count-based
standard error is binomial over the bin's suspect IDs.

According to Mickes (2015), CAC analysis is most relevant for triers of
fact (judges/jurors) evaluating estimator variables (e.g., exposure
duration, retention interval). It shows how trustworthy an ID is at
different confidence levels.

## References

Mickes, L. (2015). Receiver operating characteristic analysis and
confidence-accuracy characteristic analysis in investigations of system
variables and estimator variables that affect eyewitness memory.
*Journal of Applied Research in Memory and Cognition, 4*(2), 93-102.

Juslin, P., Olsson, N., & Winman, A. (1996). Calibration and
diagnosticity of confidence in eyewitness identification: Comments on
what can be inferred from the low confidence-accuracy correlation.
*Journal of Experimental Psychology: Learning, Memory, and Cognition,
22*(5), 1304-1316.

## Examples

``` r
data(lineup_example)
cac <- make_cacdata(lineup_example, confidence_bins = c(0, 60, 80, 100))
cac$cac_data
#> # A tibble: 3 × 6
#>   confidence n_correct n_incorrect n_total accuracy     se
#>   <chr>          <int>       <int>   <int>    <dbl>  <dbl>
#> 1 [0,60]           233          72     305    0.764 0.0243
#> 2 (60,80]           82          12      94    0.872 0.0344
#> 3 (80,100]         100           4     104    0.962 0.0189
```
