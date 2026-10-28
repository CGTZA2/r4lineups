# Compare Multiple Models for Lineup Identification Data

Fits and compares multiple models (2-HT, EIG, Full ROC) to the same
lineup identification dataset, providing a side-by-side descriptive
table.

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

- best_model: Always NULL. The included methods estimate different
  quantities and cannot be ranked by a common information criterion.

- data: The input data

- models_fit: Character vector of models successfully fit

## Details

This function provides a unified interface for fitting and comparing
multiple eyewitness identification models. It automatically handles
different data requirements and output formats across models.

\*\*Models Compared:\*\*

- \*\*2-HT (Winter et al., 2022)\*\*: Multinomial processing tree model
  with parameters for detection (dP, dA), bias (b), and guessing (g).
  Provides AIC/BIC for comparison only with other likelihood models fit
  to the same observations and outcome representation.

- \*\*EIG (Starns et al., 2023)\*\*: Information-theoretic measure of
  evidentiary value. Higher values indicate more diagnostic procedures.

- \*\*Full ROC (Smith & Yang, 2020)\*\*: Uses ALL responses to compute
  investigator discriminability. AUC ranges from 0 to 1; 0.5 represents
  chance under a fixed ordering.

\*\*Interpretation:\*\*

- AIC/BIC describe the 2-HT likelihood fit but cannot be compared with
  EIG or full-ROC AUC

- Use EIG for comparing procedure diagnosticity (higher is better)

- Use Full ROC AUC for investigator discriminability (higher is better)

## References

Winter, K., Menne, N. M., Bell, R., & Buchner, A. (2022). Experimental
validation of a multinomial processing tree model for analyzing
eyewitness identification decisions. *Scientific Reports, 12*, 15571.

Starns, J. J., Cohen, A. L., & Rotello, C. M. (2023). A complete method
for assessing the effectiveness of eyewitness identification procedures:
Expected information gain. *Psychological Review, 130*(3), 677–719.

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
#> Ordering categories by their sample diagnosticity ratios is optimistically biased: the ordering is estimated from the same data used to compute the AUC. With roughly 700 lineups per arm the bias can be substantial. Use order = "apriori" for inference or between-condition comparisons; see vignette("fullroc_analysis").
#> 
#> Model comparison complete!
print(comparison)
#> 
#> === Lineup Model Comparison ===
#> 
#> Models fit: 2ht, eig, fullroc 
#> Sample size: 1400 
#> Lineup size: 6 
#> 
#> Comparison Table:
#>                          Model                   Measure      Value
#>     2-HT (Winter et al., 2022)            Log-likelihood -1280.6300
#>     2-HT (Winter et al., 2022)                       AIC  2569.2600
#>     2-HT (Winter et al., 2022)                       BIC  2590.2400
#>     2-HT (Winter et al., 2022)   dP (detection presence)     0.5260
#>     2-HT (Winter et al., 2022)    dA (detection absence)     0.1070
#>      EIG (Starns et al., 2023) Expected Information Gain     0.2189
#>      EIG (Starns et al., 2023)    Information Efficiency    21.9000
#>      EIG (Starns et al., 2023)             Prior Entropy     1.0000
#>  Full ROC (Smith & Yang, 2020)                  Full AUC     0.7775
#>  Full ROC (Smith & Yang, 2020)          Operating Points    19.0000
#>  Full ROC (Smith & Yang, 2020)              Max Hit Rate     1.0000
#>               Interpretation
#>            Model fit quality
#>              Lower is better
#>              Lower is better
#>                       0 to 1
#>                       0 to 1
#>      Higher is better (bits)
#>          Percentage (0-100%)
#>          Maximum possible IG
#>       Higher is better (0-1)
#>  Number of decision criteria
#>             Cumulative (0-1)
#> 
#> Metrics describe different estimands and are not a single model-selection scale.
#> 
#> Access fitted models via: $fitted_models$<model_name>
#> Available models: 2ht, eig, fullroc 
summary(comparison)
#> 
#> === Model Comparison Summary ===
#> 
#> Dataset:
#>   Total observations: 1400 
#>   Target-present: 700 
#>   Target-absent: 700 
#>   Lineup size: 6 
#> 
#> Models fitted: 3 
#>    2ht, eig, fullroc 
#> 
#> --- 2-HT Model Summary ---
#>   Parameters: dP = 0.526 , dA = 0.107 , b = 0.009 , g = 0.796 
#>   AIC: 2569.26 
#>   BIC: 2590.24 
#> 
#> --- EIG Summary ---
#>   EIG: 0.2189 bits
#>   Information efficiency: 21.9 %
#>   Number of response categories: 19 
#> 
#> --- Full ROC Summary ---
#>   Full AUC: 0.7775 
#>   Operating points: 19 
#>   Ordering method: diagnosticity 
#> 
#> ---
#> Interpretation Guidance:
#>   - 2-HT: AIC/BIC are comparable only to other likelihood models fit to the same outcomes
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
#>   Target-present: 700
#>   Target-absent:  700
#>   Total:          1400
#>   Lineup size:    6
#> 
#> Parameter Estimates:
#>                       Estimate     SE
#> dP (culprit presence)   0.5262 0.0231
#> dA (culprit absence)    0.1067 0.0333
#> b  (biased selection)   0.0093 0.0164
#> g  (guessing)           0.7961 0.0227
#> 
#> Log-likelihood: -1280.63
#> AIC: 2569.26
#> BIC: 2590.24
comparison$fitted_models$eig
#> 
#> === Expected Information Gain (EIG) Analysis ===
#> 
#> EIG: 0.2189 bits
#> Prior probability of guilt: 0.5 
#> Prior entropy: 1 bits
#> Information efficiency: 21.9 %
#> 
#> Sample sizes:
#>   Target-present (guilty): 700 
#>   Target-absent (innocent): 700 
#> 
#> Top response categories by information gain:
#> (Showing up to 10 of 19 total)
#> 
#> # A tibble: 10 × 6
#>    response    identification confidence_level p_response posterior_guilty
#>    <chr>       <chr>          <chr>            <chr>      <chr>           
#>  1 suspect_100 suspect        100              0.049      1.000           
#>  2 suspect_60  suspect        60               0.049      0.928           
#>  3 suspect_70  suspect        70               0.040      0.893           
#>  4 suspect_90  suspect        90               0.026      0.889           
#>  5 suspect_80  suspect        80               0.027      0.842           
#>  6 suspect_50  suspect        50               0.054      0.776           
#>  7 suspect_40  suspect        40               0.047      0.758           
#>  8 reject_20   reject         20               0.191      0.250           
#>  9 filler_100  filler         100              0.019      0.259           
#> 10 filler_30   filler         30               0.079      0.300           
#> # ℹ 1 more variable: information_gain <chr>
#> 
#> Full response data available in $response_data
```
