# SDT Summary-Level Comparisons

## Overview

This vignette shows how to compare **conditions using only summary SDT
counts** (hits, false alarms, misses, correct rejections). The methods
follow Gourevitch & Galanter (1967), Miller (1996), and Suero et
al. (2017), with an optional bootstrap variance approach based on the
Tredoux et al. supplemental materials in
`notes/11292_2020_9432_MOESM1_ESM.docx`.

Use these tools when **trial‑level data are unavailable** and you only
have summary counts for each condition.

## Summary metrics from counts

``` r

library(r4lineups)

# Condition A counts
hits_a <- 34
fas_a <- 6
misses_a <- 16
cr_a <- 44

summ_a <- sdt_summary_from_counts(hits_a, fas_a, misses_a, cr_a)
summ_a
#> $hits
#> [1] 34
#> 
#> $fas
#> [1] 6
#> 
#> $misses
#> [1] 16
#> 
#> $cr
#> [1] 44
#> 
#> $ns
#> [1] 50
#> 
#> $nn
#> [1] 50
#> 
#> $hit_rate
#> [1] 0.6764706
#> 
#> $fa_rate
#> [1] 0.127451
#> 
#> $zH
#> [1] 0.4578519
#> 
#> $zF
#> [1] -1.138523
#> 
#> $dprime
#> [1] 1.596375
#> 
#> $c
#> [1] 0.3403358
#> 
#> $ln_beta
#> [1] 0.5433037
#> 
#> $beta
#> [1] 1.721685
#> 
#> $correction
#> [1] "loglinear"
```

## Variance estimation

Miller’s exact‑binomial method is recommended for most cases:

``` r

var_a_miller <- sdt_summary_variance(hits_a, fas_a, misses_a, cr_a, method = "miller")
var_a_miller
#> $var_zH
#> [1] 0.03372994
#> 
#> $var_zF
#> [1] 0.05230691
#> 
#> $var_dprime
#> [1] 0.08603685
#> 
#> $var_c
#> [1] 0.02150921
#> 
#> $cov_dprime_c
#> [1] 0.009288488
#> 
#> $var_ln_beta
#> [1] 0.07487284
#> 
#> $nboot
#> [1] 1000
#> 
#> $method
#> [1] "miller"
#> 
#> $correction
#> [1] "loglinear"
```

Gourevitch & Galanter’s delta method is faster but more approximate:

``` r

var_a_gouv <- sdt_summary_variance(hits_a, fas_a, misses_a, cr_a, method = "gourevitch")
var_a_gouv
#> $var_zH
#> [1] 0.03391665
#> 
#> $var_zF
#> [1] 0.05108471
#> 
#> $var_dprime
#> [1] 0.08500136
#> 
#> $var_c
#> [1] 0.02125034
#> 
#> $cov_dprime_c
#> [1] 0.008584026
#> 
#> $var_ln_beta
#> [1] 0.07332771
#> 
#> $nboot
#> [1] 1000
#> 
#> $method
#> [1] "gourevitch"
#> 
#> $correction
#> [1] "loglinear"
```

Bootstrap variance can also be used:

``` r

set.seed(1)
var_a_boot <- sdt_summary_variance(hits_a, fas_a, misses_a, cr_a,
                                   method = "bootstrap", nboot = 1000)
var_a_boot
#> $var_zH
#> [1] 0.03600002
#> 
#> $var_zF
#> [1] 0.05148473
#> 
#> $var_dprime
#> [1] 0.08391206
#> 
#> $var_c
#> [1] 0.02276436
#> 
#> $cov_dprime_c
#> [1] 0.007742356
#> 
#> $var_ln_beta
#> [1] 0.08607643
#> 
#> $nboot
#> [1] 1000
#> 
#> $method
#> [1] "bootstrap"
#> 
#> $correction
#> [1] "loglinear"
```

## Comparing two conditions

``` r

# Condition B counts
hits_b <- 28
fas_b <- 10
misses_b <- 22
cr_b <- 40

compare_sdt_summary(hits_a, fas_a, misses_a, cr_a,
                    hits_b, fas_b, misses_b, cr_b,
                    metric = "dprime",
                    method = "miller")
#> $metric
#> [1] "dprime"
#> 
#> $estimate_a
#> [1] 1.596375
#> 
#> $estimate_b
#> [1] 0.9687792
#> 
#> $diff
#> [1] 0.6275962
#> 
#> $se
#> [1] 0.3973377
#> 
#> $z
#> [1] 1.579503
#> 
#> $p_value
#> [1] 0.1142207
#> 
#> $method
#> [1] "miller"
#> 
#> $correction
#> [1] "loglinear"
```

You can also compare **criterion** or **lnβ**:

``` r

compare_sdt_summary(hits_a, fas_a, misses_a, cr_a,
                    hits_b, fas_b, misses_b, cr_b,
                    metric = "c",
                    method = "bootstrap",
                    nboot = 2000)
#> $metric
#> [1] "c"
#> 
#> $estimate_a
#> [1] 0.3403358
#> 
#> $estimate_b
#> [1] 0.3364025
#> 
#> $diff
#> [1] 0.003933284
#> 
#> $se
#> [1] 0.1999384
#> 
#> $z
#> [1] 0.01967248
#> 
#> $p_value
#> [1] 0.9843046
#> 
#> $method
#> [1] "bootstrap"
#> 
#> $correction
#> [1] "loglinear"
```

## Notes

- Extreme rates (0 or 1) are corrected by default (loglinear).
- If only **published d′ or β** are available, you need a reported
  **SE/variance** to compare conditions. Without counts or SEs, z‑tests
  are not identifiable.
