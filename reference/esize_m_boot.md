# Bootstrapped Effective Size

Base function for computing bootstrapped effective size

## Usage

``` r
esize_m_boot(lineup_vec, d, k)
```

## Arguments

- lineup_vec:

  A vector of lineup choices

- d:

  Indices for bootstrap resampling

- k:

  A vector indexing number of members in each lineup pair. Must be
  specified by user (scalar).

## Value

If printarg=FALSE, provides only Malpass's priginal calculation of
effective size

## Details

Function to call when bootstrap resampling using boot function (in
package 'boot')

## References

Davison, A.C. & Hinkley, D.V. (1997). *Bootstrap methods and their
application*. Cambridge University Press.

Malpass, R. S. (1981). Effective size and defendant bias in eyewitness
identification lineups. *Law and Human Behavior, 5*(4), 299-309.

Malpass, R. S., Tredoux, C., & McQuiston-Surrett, D. (2007). Lineup
construction and lineup fairness. In R. Lindsay, D. F. Ross, J. D. Read,
& M. P. Toglia (Eds.), *Handbook of Eyewitness Psychology, Vol. 2:
Memory for people* (pp. 155-178). Mahwah, NJ: Lawrence Erlbaum
Associates.

Tredoux, C. G. (1998). Statistical inference on measures of lineup
fairness. *Law and Human Behavior, 22*(2), 217-237.

Tredoux, C. (1999). Statistical considerations when determining measures
of lineup size and lineup bias. *Applied Cognitive Psychology*, 13,
S9-S26.

Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
empirically assessing the fairness of a lineup. *Law and Human Behavior,
3*(4), 285-293.

## See also

[`boot`](https://rdrr.io/pkg/boot/man/boot.html):
https://cran.r-project.org/web/packages/boot/boot.pdf

## Examples

``` r
#Data:
lineup_vec <- round(runif(100, 1, 6))

#Get boot object:
bootobject <- boot::boot(lineup_vec, esize_m_boot, k = 6, R=1000)
bootobject
#> 
#> ORDINARY NONPARAMETRIC BOOTSTRAP
#> 
#> 
#> Call:
#> boot::boot(data = lineup_vec, statistic = esize_m_boot, R = 1000, 
#>     k = 6)
#> 
#> 
#> Bootstrap Statistics :
#>     original   bias    std. error
#> t1*     5.46 -0.21534   0.2121728

#To get confidence intervals:
cis <- boot::boot.ci(bootobject, conf = 0.95, type = "all")
#> Warning: bootstrap variances needed for studentized intervals
#> Warning: extreme order statistics used as endpoints
```
