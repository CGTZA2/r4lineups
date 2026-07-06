# Plot ROC Curve for Lineup Identification

Creates a ggplot2 visualization of ROC data from lineup identification
experiments.

## Usage

``` r
make_roc_gg(rocobj_list, show_pauc = TRUE, point_labels = TRUE)
```

## Arguments

- rocobj_list:

  List output from make_rocdata()

- show_pauc:

  Logical. Whether to display pAUC value on plot (default = TRUE)

- point_labels:

  Logical. Whether to label points with confidence levels (default =
  TRUE)

## Value

A ggplot2 object

## Examples

``` r
data(lineup_example)
roc <- make_rocdata(lineup_example)
make_roc_gg(roc)

```
