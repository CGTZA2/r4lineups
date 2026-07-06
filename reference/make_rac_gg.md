# Plot RAC Curve

Creates a ggplot2 visualization of RAC data showing the relationship
between response time and accuracy.

## Usage

``` r
make_rac_gg(
  racobj_list,
  show_errorbars = TRUE,
  show_n = TRUE,
  time_units = "ms"
)
```

## Arguments

- racobj_list:

  List output from make_racdata()

- show_errorbars:

  Logical. Whether to show error bars (default = TRUE)

- show_n:

  Logical. Whether to show sample sizes (default = TRUE)

- time_units:

  Character. Label for time units (e.g., "ms", "seconds"). Default =
  "ms"

## Value

A ggplot2 object

## Examples

``` r
data <- create_example_lineup_data(n_trials = 200,
                                   include_response_time = TRUE,
                                   seed = 123)
rac <- make_racdata(data, time_bins = c(0, 4000, 8000, 12000, 20000))
make_rac_gg(rac)
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_line()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_text()`).

```
