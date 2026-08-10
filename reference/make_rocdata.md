# Compute ROC Data for Lineup Identification

Computes Receiver Operating Characteristic (ROC) data from lineup
identification experiments following the methodology of Mickes, Wixted,
and Gronlund.

## Usage

``` r
make_rocdata(data, lineup_size = 6)
```

## Arguments

- data:

  A dataframe with the following columns:

  - target_present: Logical. TRUE if guilty suspect in lineup, FALSE if
    innocent

  - identification: Character. "suspect", "filler", or "reject"

  - confidence: Numeric. Confidence rating (higher = more confident)

- lineup_size:

  Integer. Number of people in the lineup (default = 6). Used to
  estimate false ID rate when fillers are chosen from target-absent
  lineups.

## Value

A list containing:

- roc_data: Dataframe with correct_id_rate, false_id_rate, and
  confidence

- pauc: Partial area under the curve

- n_target_present: Number of target-present lineups

- n_target_absent: Number of target-absent lineups

## Details

This function computes ROC curves following Wixted & Mickes (2012) and
Mickes (2015). For each confidence level:

- Correct ID rate = proportion of target-present lineups where suspect
  identified

- False ID rate = proportion of target-absent lineups where suspect
  identified

If target-absent data contain explicit `"suspect"` responses, those
designated innocent-suspect IDs are used directly. Otherwise, filler IDs
are divided by lineup size. The two estimators are never added together.

## References

Wixted, J. T., & Mickes, L. (2012). The field of eyewitness memory
should abandon probative value and embrace receiver operating
characteristic analysis. *Perspectives on Psychological Science, 7*(3),
275-278.

Mickes, L. (2015). Receiver operating characteristic analysis and
confidence-accuracy characteristic analysis in investigations of system
variables and estimator variables that affect eyewitness memory.
*Journal of Applied Research in Memory and Cognition, 4*(2), 93-102.

Gronlund, S. D., Wixted, J. T., & Mickes, L. (2014). Evaluating
eyewitness identification procedures using receiver operating
characteristic analysis. *Current Directions in Psychological Science,
23*(1), 3-10.

## Examples

``` r
data(lineup_example)
roc <- make_rocdata(lineup_example)
roc$roc_data
#> # A tibble: 10 × 5
#>    confidence correct_id_rate false_id_rate n_correct_ids n_false_ids
#>         <dbl>           <dbl>         <dbl>         <dbl>       <dbl>
#>  1         19            0             0                0           0
#>  2        100            0.17          0               17           0
#>  3         90            0.38          0               38           0
#>  4         80            0.48          0.06            48           6
#>  5         70            0.55          0.1             55          10
#>  6         60            0.6           0.11            60          11
#>  7         50            0.6           0.15            60          15
#>  8         40            0.6           0.15            60          15
#>  9         30            0.6           0.15            60          15
#> 10         20            0.6           0.15            60          15
roc$pauc
#> [1] 0.07615
```
