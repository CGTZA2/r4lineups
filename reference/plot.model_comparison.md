# Plot Side-by-Side Model Comparisons

Creates a multi-panel visualization comparing fitted models.

## Usage

``` r
# S3 method for class 'model_comparison'
plot(x, which = "all", ncol = 2, ...)
```

## Arguments

- x:

  A model_comparison object from compare_models()

- which:

  Character vector specifying which plots to create. Options: "2ht",
  "eig_ig", "eig_posteriors", "fullroc" Default = "all" creates all
  available plots.

- ncol:

  Integer. Number of columns for plot layout (default = 2)

- ...:

  Additional arguments passed to individual plotting functions

## Value

A combined ggplot object (via patchwork or cowplot)

## Details

This function creates side-by-side visualizations of all fitted models
for easy comparison. The specific plots depend on which models were fit:

- \*\*2-HT\*\*: Parameter estimates with confidence intervals

- \*\*EIG\*\*: Information gain by response category

- \*\*Full ROC\*\*: ROC curve with AUC
