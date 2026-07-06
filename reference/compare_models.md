# Compare Multiple Models for Lineup Identification Data

Fits and compares multiple models (2-HT, EIG, Full ROC) to the same
lineup identification dataset, providing a comprehensive comparison
table and model selection recommendations.

## Usage

``` r
compare_models(
  data,
  models = c("2ht", "eig", "fullroc"),
  lineup_size = 6,
  prior_guilt = 0.5,
  confidence_bins = NULL,
  show_warnings = FALSE,
  ...
)
```

## Arguments

- data:

  A dataframe with columns: target_present, identification, confidence

- models:

  Character vector of models to fit. Options:

  - "2ht" or "winter": Winter et al. (2022) Two-High Threshold MPT model

  - "eig": Expected Information Gain (Starns et al., 2023)

  - "fullroc": Full ROC curve (Smith & Yang, 2020)

  Default = c("2ht", "eig", "fullroc") fits all models.

- lineup_size:

  Integer. Number of people in the lineup (default = 6)

- prior_guilt:

  Numeric. Prior probability of guilt for EIG (default = 0.5)

- confidence_bins:

  Numeric vector of confidence bin edges (optional). Used for EIG and
  Full ROC if specified.

- show_warnings:

  Logical. Whether to show model fitting warnings (default = FALSE)

- ...:

  Additional arguments passed to individual model functions

## Value

An object of class "model_comparison" containing:

- comparison_table: Dataframe comparing model fits

- fitted_models: List of fitted model objects

- best_model: Name of best model by AIC (if applicable)

- data: The input data

- models_fit: Character vector of models successfully fit

## Details

This function provides a unified interface for fitting and comparing
multiple eyewitness identification models. It automatically handles
different data requirements and output formats across models.

\*\*Models Compared:\*\*

- \*\*2-HT (Winter et al., 2022)\*\*: Multinomial processing tree model
  with parameters for detection (dP, dA), bias (b), and guessing (g).
  Provides AIC/BIC for model comparison.

- \*\*EIG (Starns et al., 2023)\*\*: Information-theoretic measure of
  evidentiary value. Higher values indicate more diagnostic procedures.

- \*\*Full ROC (Smith & Yang, 2020)\*\*: Uses ALL responses to compute
  investigator discriminability. AUC ranges from 0.5 (chance) to 1.0
  (perfect).

\*\*Model Selection:\*\*

- Use AIC/BIC for 2-HT model (lower is better)

- Use EIG for comparing procedure diagnosticity (higher is better)

- Use Full ROC AUC for investigator discriminability (higher is better)

## References

Winter, K., Menne, N. M., Bell, R., & Buchner, A. (2022). Experimental
validation of a multinomial processing tree model for analyzing
eyewitness identification decisions. *Scientific Reports, 12*, 15571.

Starns, J. J., Chen, T., & Staub, A. (2023). Assessing theoretical
conclusions via the data they should have produced. *Psychological
Review*.

Smith, A. M., Yang, Y., & Wells, G. L. (2020). Distinguishing between
investigator discriminability and eyewitness discriminability.
*Perspectives on Psychological Science, 15*(3), 589-607.

## Examples

``` r
data(lineup_example)
# Fit all models
comparison <- compare_models(lineup_example)
#> Fitting 2-HT model...
#> Computing EIG...
#> Computing Full ROC...
#> 
#> Model comparison complete!
print(comparison)
#> 
#> === Lineup Model Comparison ===
#> 
#> Models fit: 2ht, eig, fullroc 
#> Sample size: 200 
#> Lineup size: 6 
#> 
#> Comparison Table:
#>                          Model                   Measure     Value
#>     2-HT (Winter et al., 2022)            Log-likelihood -187.6900
#>     2-HT (Winter et al., 2022)                       AIC  383.3700
#>     2-HT (Winter et al., 2022)                       BIC  396.5700
#>     2-HT (Winter et al., 2022)   dP (detection presence)    0.4870
#>     2-HT (Winter et al., 2022)    dA (detection absence)    0.3170
#>      EIG (Starns et al., 2023) Expected Information Gain    0.3691
#>      EIG (Starns et al., 2023)    Information Efficiency   36.9000
#>      EIG (Starns et al., 2023)             Prior Entropy    1.0000
#>  Full ROC (Smith & Yang, 2020)                  Full AUC    0.8589
#>  Full ROC (Smith & Yang, 2020)          Operating Points   15.0000
#>  Full ROC (Smith & Yang, 2020)              Max Hit Rate    1.0000
#>               Interpretation
#>            Model fit quality
#>              Lower is better
#>              Lower is better
#>                       0 to 1
#>                       0 to 1
#>      Higher is better (bits)
#>          Percentage (0-100%)
#>          Maximum possible IG
#>   Higher is better (0.5-1.0)
#>  Number of decision criteria
#>             Cumulative (0-1)
#> 
#> 
#> Best parametric model (by AIC): 2ht 
#> 
#> Access fitted models via: $fitted_models$<model_name>
#> Available models: 2ht, eig, fullroc 
summary(comparison)
#> 
#> === Model Comparison Summary ===
#> 
#> Dataset:
#>   Total observations: 200 
#>   Target-present: 100 
#>   Target-absent: 100 
#>   Lineup size: 6 
#> 
#> Models fitted: 3 
#>    2ht, eig, fullroc 
#> 
#> --- 2-HT Model Summary ---
#>   Parameters: dP = 0.487 , dA = 0.317 , b = 0.149 , g = 0.495 
#>   AIC: 383.37 
#>   BIC: 396.57 
#> 
#> --- EIG Summary ---
#>   EIG: 0.3691 bits
#>   Information efficiency: 36.9 %
#>   Number of response categories: 15 
#> 
#> --- Full ROC Summary ---
#>   Full AUC: 0.8589 
#>   Operating points: 15 
#>   Ordering method: diagnosticity 
#> 
#> ---
#> Model Selection Guidance:
#>   - 2-HT: Use AIC/BIC for parametric model comparison
#>   - EIG: Higher values = more informative procedure
#>   - Full ROC: Higher AUC = better investigator discriminability
#> 
#> Each model provides different insights -- consider using multiple models.

# Fit specific models
comparison <- compare_models(lineup_example, models = c("2ht", "eig"))
#> Fitting 2-HT model...
#> Computing EIG...
#> 
#> Model comparison complete!

# Access individual fitted models
comparison$fitted_models$`2ht`
#> 
#> Winter et al. (2022) Two-High Threshold MPT Model
#> ==================================================
#> 
#> Sample size:
#>   Target-present: 100
#>   Target-absent:  100
#>   Total:          200
#>   Lineup size:    6
#> 
#> Parameter Estimates:
#>                       Estimate     SE
#> dP (culprit presence)   0.4875 0.0755
#> dA (culprit absence)    0.3167 0.1318
#> b  (biased selection)   0.1493 0.0648
#> g  (guessing)           0.4954 0.0794
#> 
#> Log-likelihood: -187.69
#> AIC: 383.37
#> BIC: 396.57
comparison$fitted_models$eig
#> 
#> === Expected Information Gain (EIG) Analysis ===
#> 
#> EIG: 0.3691 bits
#> Prior probability of guilt: 0.5 
#> Prior entropy: 1 bits
#> Information efficiency: 36.9 %
#> 
#> Sample sizes:
#>   Target-present (guilty): 100 
#>   Target-absent (innocent): 100 
#> 
#> Top response categories by information gain:
#> (Showing up to 10 of 15 total)
#> 
#> # A tibble: 10 × 6
#>    response    identification confidence_level p_response posterior_guilty
#>    <chr>       <chr>          <chr>            <chr>      <chr>           
#>  1 suspect_100 suspect        100              0.085      1.000           
#>  2 suspect_90  suspect        90               0.105      1.000           
#>  3 reject_60   reject         60               0.045      0.000           
#>  4 suspect_50  suspect        50               0.020      0.000           
#>  5 filler_70   filler         70               0.040      0.125           
#>  6 reject_40   reject         40               0.120      0.167           
#>  7 suspect_60  suspect        60               0.030      0.833           
#>  8 reject_20   reject         20               0.060      0.250           
#>  9 reject_50   reject         50               0.105      0.286           
#> 10 filler_60   filler         60               0.050      0.300           
#> # ℹ 1 more variable: information_gain <chr>
#> 
#> Full response data available in $response_data
```
