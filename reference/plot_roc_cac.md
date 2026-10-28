# Plot Methods for ROC and CAC Objects

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods for the
objects returned by the ROC and CAC functions, so that `plot(x)` works
directly rather than requiring the user to know which plot builder
matches which object.

## Usage

``` r
# S3 method for class 'lineup_roc_data'
plot(x, ...)

# S3 method for class 'lineup_roc'
plot(x, ...)

# S3 method for class 'lineup_cac_data'
plot(x, ...)

# S3 method for class 'lineup_cac'
plot(x, ...)

# S3 method for class 'lineup_fullroc_data'
plot(x, ...)

# S3 method for class 'lineup_fullroc'
plot(x, ...)
```

## Arguments

- x:

  An object returned by one of the ROC or CAC functions listed above.

- ...:

  Additional arguments passed to the underlying plot builder.

## Value

A ggplot2 object.

## Details

Each ROC/CAC analysis comes in two forms. The `*_data()` function
computes and returns the data only; the function without the suffix
wraps it, builds the plot, and returns a classed object with the plot
stored in `$plot`:

- [`make_rocdata`](https://cgtza2.github.io/r4lineups/reference/make_rocdata.md)
  /
  [`make_roc`](https://cgtza2.github.io/r4lineups/reference/make_roc.md)

- [`make_cacdata`](https://cgtza2.github.io/r4lineups/reference/make_cacdata.md)
  /
  [`make_cac`](https://cgtza2.github.io/r4lineups/reference/make_cac.md)

- [`make_fullroc_data`](https://cgtza2.github.io/r4lineups/reference/make_fullroc_data.md)
  /
  [`make_fullroc`](https://cgtza2.github.io/r4lineups/reference/make_fullroc.md)

These methods accept either form. When a plot has already been built it
is returned as-is; otherwise it is constructed with the matching builder
([`make_roc_gg`](https://cgtza2.github.io/r4lineups/reference/make_roc_gg.md),
[`make_cac_gg`](https://cgtza2.github.io/r4lineups/reference/make_cac_gg.md)
or
[`plot_fullroc`](https://cgtza2.github.io/r4lineups/reference/plot_fullroc.md)).

The value is an ordinary ggplot object, so it can be modified with the
usual `+` syntax, e.g. `plot(x) + ggplot2::theme_minimal()`.

## Examples

``` r
data(lineup_example)
plot(make_rocdata(lineup_example))

plot(make_cacdata(lineup_example))

```
