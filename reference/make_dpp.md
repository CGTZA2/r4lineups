# Compute Deviation from Perfect Performance (DPP)

Computes DPP metric following Smith et al. (2018). DPP measures how much
an observed ROC curve deviates from perfect performance, providing a
single-number summary that is less affected by ROC truncation than pAUC.

## Usage

``` r
make_dpp(data, lineup_size = 6, use_roc_obj = FALSE)
```

## Arguments

- data:

  A dataframe with the following columns:

  - target_present: Logical. TRUE if guilty suspect in lineup

  - identification: Character. "suspect", "filler", or "reject"

  - confidence: Numeric. Confidence rating

- lineup_size:

  Integer. Number of people in lineup (default = 6)

- use_roc_obj:

  Logical. If FALSE (default), computes ROC from data. If TRUE, data
  should be the output from make_rocdata().

## Value

A list containing:

- dpp: Deviation from Perfect Performance (0 = perfect, 1 = worst)

- auc_observed: Area under observed ROC curve

- auc_perfect: Area under perfect ROC curve (for same FA range)

- auc_gap: Raw area gap, \\AUC\_{perfect} - AUC\_{observed}\\

- roc_data: ROC curve data

- max_fa: Maximum false alarm rate in observed data

## Details

DPP (Deviation from Perfect Performance) compares the observed ROC curve
to a perfect ROC curve within the same false alarm rate range:

\$\$DPP = 1 - \frac{AUC\_{observed}}{AUC\_{perfect}}\$\$

Where:

- **Perfect ROC**: Goes from (0,0) → (0,1) → (max_FA,1) (immediate jump
  to 100

- **Observed ROC**: Actual performance from data

- **AUC**: Area under curve computed via trapezoidal rule

\*\*Interpretation\*\*:

- DPP = 0: Perfect performance (all correct IDs at 0

- DPP → 1: Very poor performance (near chance)

- Lower DPP = better performance

\*\*Advantages over pAUC\*\* (Smith et al. 2018):

- Less affected by ROC truncation (different confidence distributions)

- Normalized relative to best achievable performance

- More consistent rankings across datasets

- Accounts for achievable performance given observed FA range

The perfect ROC is constrained to the same FA range as the observed
data, making DPP a fair measure even when procedures produce different
confidence distributions (and thus different ROC truncation points).

## References

Smith, A. M., Wilford, M. M., Quigley-McBride, A., & Wells, G. L.
(2019). Mistaken eyewitness identification rates increase when either
witnessing or testing conditions get worse. *Law and Human Behavior,
43*(4), 358-368.

Smith, A. M., et al. (2018). Deviation from perfect performance measures
the diagnostic utility of eyewitness lineups but partial area under the
ROC does not. *Journal of Applied Research in Memory and Cognition*.

## Examples

``` r
data(lineup_example)
dpp <- make_dpp(lineup_example)
dpp$dpp
#> [1] 0.5396491
```
