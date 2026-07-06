# Plot Full ROC Curve

Creates a ggplot2 visualization of full ROC data from lineup
identification experiments using the Smith & Yang (2020) method.

## Usage

``` r
plot_fullroc(
  fullroc_obj,
  show_auc = TRUE,
  point_labels = FALSE,
  title = "Full ROC Curve (Smith & Yang, 2020)"
)
```

## Arguments

- fullroc_obj:

  List output from make_fullroc_data()

- show_auc:

  Logical. Whether to display AUC value on plot (default = TRUE)

- point_labels:

  Logical. Whether to label points (default = FALSE, can be crowded)

- title:

  Character. Plot title (default = "Full ROC Curve (Smith & Yang,
  2020)")

## Value

A ggplot2 object

## Examples

``` r
data(lineup_example)
fullroc_result <- make_fullroc_data(lineup_example)
plot_fullroc(fullroc_result)

```
