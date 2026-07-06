# Create Comprehensive Model Comparison Table

Creates a formatted table comparing model fits for publication or
reporting.

## Usage

``` r
format_comparison_table(comparison_obj, format = "console", digits = 3)
```

## Arguments

- comparison_obj:

  A model_comparison object from compare_models()

- format:

  Character. Output format: "console" (default), "markdown", "latex"

- digits:

  Integer. Number of decimal places (default = 3)

## Value

A formatted table (character string or dataframe)

## Details

Creates a publication-ready table with key statistics for each model:

- 2-HT: Parameters (dP, dA, b, g), AIC, BIC

- EIG: Information gain, efficiency

- Full ROC: AUC

## Examples

``` r
data(lineup_example)
comparison <- compare_models(lineup_example)
#> Fitting 2-HT model...
#> Computing EIG...
#> Computing Full ROC...
#> 
#> Model comparison complete!
format_comparison_table(comparison)
#>                            Model                   Measure     Value
#> 1     2-HT (Winter et al., 2022)            Log-likelihood -187.6900
#> 2     2-HT (Winter et al., 2022)                       AIC  383.3700
#> 3     2-HT (Winter et al., 2022)                       BIC  396.5700
#> dP    2-HT (Winter et al., 2022)   dP (detection presence)    0.4870
#> dA    2-HT (Winter et al., 2022)    dA (detection absence)    0.3170
#> 11     EIG (Starns et al., 2023) Expected Information Gain    0.3691
#> 12     EIG (Starns et al., 2023)    Information Efficiency   36.9000
#> 13     EIG (Starns et al., 2023)             Prior Entropy    1.0000
#> 14 Full ROC (Smith & Yang, 2020)                  Full AUC    0.8589
#> 15 Full ROC (Smith & Yang, 2020)          Operating Points   15.0000
#> 16 Full ROC (Smith & Yang, 2020)              Max Hit Rate    1.0000
#>                 Interpretation
#> 1            Model fit quality
#> 2              Lower is better
#> 3              Lower is better
#> dP                      0 to 1
#> dA                      0 to 1
#> 11     Higher is better (bits)
#> 12         Percentage (0-100%)
#> 13         Maximum possible IG
#> 14  Higher is better (0.5-1.0)
#> 15 Number of decision criteria
#> 16            Cumulative (0-1)
```
