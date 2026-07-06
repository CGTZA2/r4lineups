# mSDT: Max Filler Distribution (IID Normal Fillers)

Distribution functions for the maximum of (m-1) independent standard
normal filler signals, where m is the lineup size (suspect + fillers).

## Usage

``` r
dmax_filler(x, lineup_size, log = FALSE)

pmax_filler(q, lineup_size, lower.tail = TRUE, log.p = FALSE)

qmax_filler(p, lineup_size, lower.tail = TRUE, log.p = FALSE)

rmax_filler(n, lineup_size)
```

## Arguments

- x, q:

  Numeric vector of quantiles.

- lineup_size:

  Lineup size (suspect + fillers), must be \>= 2.

- log, log.p:

  Logical. Return log-density / log-probability.

- lower.tail:

  Logical. If TRUE (default), returns P\[X \<= q\].

- p:

  Numeric vector of probabilities.

- n:

  Number of random draws.

## Value

Density, distribution, quantile, or random draws.

## Examples

``` r
dmax_filler(0, lineup_size = 6)
#> [1] 0.1246695
pmax_filler(0, lineup_size = 6)
#> [1] 0.03125
qmax_filler(0.5, lineup_size = 6)
#> [1] 1.128998
rmax_filler(5, lineup_size = 6)
#> [1] 1.141755 1.077630 1.357769 0.962494 1.592569
```
