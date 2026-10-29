# Plot DPP Comparison Between Two Procedures

Overlays DPP at each operating point for two procedures.

## Usage

``` r
plot_dpp_comparison(
  compare_obj,
  proc_a_label = "Procedure A",
  proc_b_label = "Procedure B"
)
```

## Arguments

- compare_obj:

  List output from compare_dpp()

- proc_a_label:

  Character. Label for procedure A (default = "Procedure A")

- proc_b_label:

  Character. Label for procedure B (default = "Procedure B")

## Value

A ggplot2 object

## Examples

``` r
data(lineup_example)
odd <- seq(1, nrow(lineup_example), by = 2)
cmp <- compare_dpp(lineup_example[odd, ], lineup_example[-odd, ],
                   n_bootstrap = 0)
plot_dpp_comparison(cmp)

```
