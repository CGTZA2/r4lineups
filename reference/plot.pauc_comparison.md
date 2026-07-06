# Plot pAUC Comparison

Creates a side-by-side visualization of ROC curves for two conditions
with shaded pAUC regions and statistical test results.

## Usage

``` r
# S3 method for class 'pauc_comparison'
plot(x, show_cutoff = TRUE, show_test_results = TRUE, ...)
```

## Arguments

- x:

  A pauc_comparison object from compare_pauc()

- show_cutoff:

  Logical. Whether to show false ID rate cutoff line (default = TRUE)

- show_test_results:

  Logical. Whether to show test results on plot (default = TRUE)

- ...:

  Additional arguments (ignored)

## Value

A ggplot2 object

## Details

Creates a comparison plot showing:

- ROC curves for both conditions

- Shaded pAUC regions (up to cutoff)

- Vertical line at false ID rate cutoff

- Test statistics (Z-score, p-value)
