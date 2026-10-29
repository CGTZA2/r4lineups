# Compute Deviation from Perfect Performance (DPP)

Computes the average deviation from perfect performance of Smith,
Lampinen, Wells, Smalarz and Mackovichova (2019). DPP is calculated at
every operating point (cumulative confidence threshold) of the ROC and
averaged over them, so that, unlike pAUC, it is not tied to a region of
false-ID rates that two differently truncated ROC curves may not share.

## Usage

``` r
make_dpp(
  data,
  lineup_size = 6,
  use_roc_obj = FALSE,
  alpha = NULL,
  beta = NULL,
  innocent_suspect = "auto",
  thresholds = NULL
)
```

## Arguments

- data:

  A dataframe with the following columns:

  - target_present: Logical. TRUE if guilty suspect in lineup

  - identification: Character. "suspect", "filler", or "reject"

  - confidence: Numeric. Confidence rating

  Alternatively, with `use_roc_obj = TRUE`, the output of
  [`make_rocdata`](https://cgtza2.github.io/r4lineups/reference/make_rocdata.md).

- lineup_size:

  Integer. Number of people in lineup (default = 6)

- use_roc_obj:

  Logical. If FALSE (default), computes ROC from data. If TRUE, data
  should be the output from make_rocdata(), whose first `roc_data` row
  is the origin and is not an operating point.

- alpha, beta:

  Optional weights for Smith et al.'s Equation 3. `alpha` is the cost of
  an innocent-suspect identification relative to a missed culprit
  identification, and `beta` is the base rate of culprit presence.
  Supply both or neither. When both are `NULL` (default), the unweighted
  Equation 2 is used.

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

- thresholds:

  Optional numeric vector of confidence criteria at which to evaluate
  the ROC. The default uses the confidence levels of the responses that
  can move an operating point; see Details. Supplying the criteria of a
  full sample keeps them fixed across bootstrap resamples.

## Value

A list containing:

- dpp: Average DPP over the operating points

- dpp_by_point: Tibble with confidence threshold, culprit (correct-ID)
  rate, innocent-suspect ID rate and DPP at each operating point

- best_point: The operating point with the lowest DPP, which is the
  point of maximum utility

- roc_data: ROC curve data from
  [`make_rocdata()`](https://cgtza2.github.io/r4lineups/reference/make_rocdata.md)

- alpha, beta: The weights used (`NULL` for Equation 2)

- n_target_present, n_target_absent: Sample sizes

- innocent_suspect_method: How innocent-suspect IDs were obtained

- thresholds: The confidence criteria of the operating points

## Details

At an operating point \\c\\, Smith et al.'s (2019) Equation 2 is

\$\$DPP(c) = 1 - Culprit(c) + Innocent(c)\$\$

where \\Culprit(c)\\ is the culprit (correct) identification rate and
\\Innocent(c)\\ the innocent-suspect identification rate at that point.
DPP(c) ranges from 0 (perfect performance: every culprit identified, no
innocent suspect identified) to 2 (the reverse). Any point on the chance
diagonal has DPP(c) = 1. When the base rate of culprit presence is .5
and the costs of the two errors are equal, DPP(c) = 1 - U(c), where
\\U(c) = Culprit(c) - Innocent(c)\\ is the expected utility.

The reported `dpp` is the mean of DPP(c) over the operating points, one
for each confidence criterion of the ROC, from the highest to the lowest
(which includes every identification). Lower values are better. The
criteria are the confidence levels of the responses that enter the ROC
(see
[`make_rocdata`](https://cgtza2.github.io/r4lineups/reference/make_rocdata.md)),
so the confidence given to rejections does not add points or change the
average.

Equation 3 weights the two errors:

\$\$DPP\_{\alpha\beta}(c) = \beta\[1 - Culprit(c)\] + \alpha(1 -
\beta)Innocent(c)\$\$

With `alpha = 1` and `beta = 0.5` this is half of Equation 2. Weighted
values no longer range from 0 to 2.

## References

Smith, A. M., Lampinen, J. M., Wells, G. L., Smalarz, L., &
Mackovichova, S. (2019). Deviation from perfect performance measures the
diagnostic utility of eyewitness lineups but partial area under the ROC
curve does not. *Journal of Applied Research in Memory and Cognition,
8*(1), 50-59.
[doi:10.1016/j.jarmac.2018.09.003](https://doi.org/10.1016/j.jarmac.2018.09.003)

## Examples

``` r
data(lineup_example)
dpp <- make_dpp(lineup_example)
dpp$dpp
#> [1] 0.7001587
dpp$dpp_by_point
#> # A tibble: 9 × 4
#>   confidence culprit_rate innocent_rate   dpp
#>        <dbl>        <dbl>         <dbl> <dbl>
#> 1        100       0.0971       0       0.903
#> 2         90       0.143        0.00571 0.863
#> 3         80       0.189        0.0143  0.826
#> 4         70       0.26         0.0229  0.763
#> 5         60       0.351        0.03    0.679
#> 6         50       0.436        0.0543  0.619
#> 7         40       0.507        0.0771  0.57 
#> 8         30       0.556        0.103   0.547
#> 9         20       0.593        0.126   0.533

# Innocent-suspect IDs twice as costly as misses, culprit base rate .5
make_dpp(lineup_example, alpha = 2, beta = 0.5)$dpp
#> [1] 0.374127
```
