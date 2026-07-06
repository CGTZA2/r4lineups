# Diagnosticity Ratio (Wells & Lindsay, 1980; Wells & Turtle, 1986)

Computes Wells' diagnosticity ratio for one lineup pair

## Usage

``` r
diag_ratio_W(lineup_pres, lineup_abs, pos_pres, pos_abs, k1, k2)
```

## Arguments

- lineup_pres:

  A numeric vector of lineup choices for a lineup in which the target
  was present

- lineup_abs:

  A numeric vector of lineup choices for a lineup in which the target
  was absent

- pos_pres:

  A scalar, representing target position in TP lineup. Must be declared
  by user

- pos_abs:

  A scalar, representing target position in TA lineup. Must be declared
  by user

- k1:

  Number of targets in TP lineup. Must be specified by user (scalar).

- k2:

  Number of targets in TA lineup. Must be specified by user (scalar).

## References

Wells, G. L., & Lindsay, R. C. L. (1980).On estimating the diagnosticity
of eyewitness nonidentifications.*Psychological Bulletin, 88*, 776-784.

Wells, G. L., & Turtle, J. W. (1986). Eyewitness identification: The
importance of lineup models. *Psychological Bulletin, 99*, 320-329.

## Examples

``` r
#Data:
lineup_pres <- round(runif(100, 1, 6))
lineup_abs <- round(runif(70, 1, 5))
pos_pres <- 3
pos_abs <- 5

#Call:
diag_ratio_W(lineup_pres, lineup_abs, pos_pres, pos_abs, 6, 5)
#> [1] 1.6625
diag_ratio_W(lineup_pres, lineup_abs, 3, 5, 6, 5)
#> [1] 1.6625
```
