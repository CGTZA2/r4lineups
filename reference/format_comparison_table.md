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

For `format = "console"`, the comparison dataframe. For `"markdown"` and
`"latex"`, a `knitr_kable` object, which is a character vector with
\*\*one element per line\*\*. Print it with `cat(x, sep = "\n")` inside
a chunk using `results = "asis"`; plain `cat(x)` joins the lines with
spaces and the table collapses into a single paragraph.

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
#> Ordering categories by their sample diagnosticity ratios is optimistically biased: the ordering is estimated from the same data used to compute the AUC. With roughly 700 lineups per arm the bias can be substantial. Use order = "apriori" for inference or between-condition comparisons; see vignette("fullroc_analysis").
#> 
#> Model comparison complete!
format_comparison_table(comparison)
#>                            Model                   Measure      Value
#> 1     2-HT (Winter et al., 2022)            Log-likelihood -1280.6300
#> 2     2-HT (Winter et al., 2022)                       AIC  2569.2600
#> 3     2-HT (Winter et al., 2022)                       BIC  2590.2400
#> dP    2-HT (Winter et al., 2022)   dP (detection presence)     0.5260
#> dA    2-HT (Winter et al., 2022)    dA (detection absence)     0.1070
#> 11     EIG (Starns et al., 2023) Expected Information Gain     0.2189
#> 12     EIG (Starns et al., 2023)    Information Efficiency    21.9000
#> 13     EIG (Starns et al., 2023)             Prior Entropy     1.0000
#> 14 Full ROC (Smith & Yang, 2020)                  Full AUC     0.7775
#> 15 Full ROC (Smith & Yang, 2020)          Operating Points    19.0000
#> 16 Full ROC (Smith & Yang, 2020)              Max Hit Rate     1.0000
#>                 Interpretation
#> 1            Model fit quality
#> 2              Lower is better
#> 3              Lower is better
#> dP                      0 to 1
#> dA                      0 to 1
#> 11     Higher is better (bits)
#> 12         Percentage (0-100%)
#> 13         Maximum possible IG
#> 14      Higher is better (0-1)
#> 15 Number of decision criteria
#> 16            Cumulative (0-1)
```
