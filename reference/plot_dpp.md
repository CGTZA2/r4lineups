# Plot DPP Across Operating Points

Plots DPP at each operating point of the ROC against the confidence
threshold that defines it, with the average marked.

## Usage

``` r
plot_dpp(dpp_obj, show_mean = TRUE, show_chance = TRUE, show_dpp = TRUE)
```

## Arguments

- dpp_obj:

  List output from make_dpp() or compute_dpp()

- show_mean:

  Logical. Draw the average DPP as a dashed line (default = TRUE)

- show_chance:

  Logical. Draw the chance reference at DPP = 1, which applies to the
  unweighted measure only (default = TRUE)

- show_dpp:

  Logical. Display the average DPP value on the plot (default = TRUE)

## Value

A ggplot2 object

## Details

Points are ordered from the strictest threshold (highest confidence,
left) to the most lenient (right), as in Smith et al.'s (2019) Table 4.
The operating point with the lowest DPP, the point of maximum utility,
is highlighted.

## Examples

``` r
data(lineup_example)
dpp <- make_dpp(lineup_example)
plot_dpp(dpp)

```
