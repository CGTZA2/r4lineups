# Plot a Multi-d' Analysis

Plot a Multi-d' Analysis

## Usage

``` r
# S3 method for class 'multid'
plot(x, type = c("decomposition", "indices", "confidence"), ...)
```

## Arguments

- x:

  An object of class `"multid"` from
  [`make_multid`](https://cgtza2.github.io/r4lineups/reference/make_multid.md).

- type:

  `"decomposition"` (default) shows d'(GFp), -d'(IFa) and -d'(FaFp) as a
  waterfall summing to d'(GI). `"indices"` plots each index, with
  bootstrap intervals when available. `"confidence"` plots the
  cumulative indices against confidence threshold (requires
  `by_confidence = TRUE`).

- ...:

  Unused.

## Value

A ggplot object.

## Examples

``` r
md <- make_multid(counts = c(G = 45, Fp = 24, Rp = 31, I = 17, Fa = 26, Ra = 57))
plot(md)

plot(md, type = "indices")
```
