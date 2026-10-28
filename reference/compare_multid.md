# Compare Multi-d' Indices Between Two Conditions

Computes the difference (`x` minus `y`) on every Multi-d' index, for
example simultaneous versus sequential lineups, or high- versus
low-similarity lineups. When both objects carry bootstrap draws,
standard errors, percentile intervals and normal-approximation p-values
are given for each difference. The two conditions are treated as
independent samples.

## Usage

``` r
compare_multid(x, y, labels = c("x", "y"))
```

## Arguments

- x, y:

  Objects of class `"multid"` from
  [`make_multid`](https://cgtza2.github.io/r4lineups/reference/make_multid.md),
  fitted with the same `filler_rate`.

- labels:

  Names for the two conditions.

## Value

A data frame of class `"multid_comparison"`.

## Examples

``` r
# Simultaneous vs sequential lineups, Lee & Penrod's (2019) Table 3
sim <- make_multid(counts = c(G = 52, Fp = 25, Rp = 24, I = 28, Fa = 26, Ra = 46),
                   correction = "none")
sqn <- make_multid(counts = c(G = 44, Fp = 19, Rp = 39, I = 15, Fa = 17, Ra = 68),
                   correction = "none")
compare_multid(sim, sqn, labels = c("simultaneous", "sequential"))
#> Multi-d' comparison: simultaneous minus sequential
#> 
#>        index simultaneous sequential difference
#>           GI        0.620      0.864     -0.243
#>          GFp        0.720      0.719      0.001
#>          IFa        0.061     -0.082      0.143
#>         FaFp        0.039     -0.062      0.101
#>         RaRp        0.614      0.767     -0.153
#>      tp_bias        0.106     -0.048      0.154
#>    siphoning        0.659      0.801     -0.142
#>  ifa_vs_fair        1.276      0.908      0.367
```
