# Effective Size (Tredoux, 1998)

Function for generating Tredoux's transformed index-of-diversity
effective size.

## Usage

``` r
esize_T(lineup_tab, unbiased = FALSE)
```

## Arguments

- lineup_tab:

  A table of lineup choices

- unbiased:

  Logical. If `FALSE` (the default) the plug-in estimator is returned,
  as in Tredoux (1998). If `TRUE`, the small-sample bias correction
  described below is applied. The default is unchanged so that existing
  analyses and published values are unaffected.

## Details

Tredoux (1998) proposed using Agresti and Agresti's (1978) index of
diversity for lineup choices, \$\$I = 1 - \frac{\sum_i o_i^2}{N^2},\$\$
where \\o_i\\ is the observed count for lineup member \\i\\ and \\N\\ is
the total number of choices. The index is then transformed back to an
effective-size scale: \$\$E' = \frac{1}{1-I} = \frac{N^2}{\sum_i o_i^2}
= \frac{1}{\sum_i p_i^2}.\$\$ This formula is algebraically equivalent
to a reciprocal concentration form.

## Small-sample bias

The expression above is a *plug-in* estimator: sample proportions are
substituted for the unknown \\p_i\\. The plug-in concentration \\\sum_i
\hat p_i^2\\ is biased upward. Its reciprocal \\E'\\ is biased
**downward** for fair and near-fair lineups, but the direction is not
guaranteed: when one member draws most choices the bias can be upward
(two members, \\p = (.9, .1)\\, \\N = 10\\: mean 1.230 against a true
1.220, by exact enumeration). For \\N\\ choices spread evenly over \\k\\
members the bias is approximately \$\$E\[\hat{E}'\] \approx
\frac{Nk}{N + k - 1}, \qquad \mathrm{bias} = -\frac{k(k-1)}{N + k -
1}.\$\$ A perfectly fair six-person lineup assessed with 50 choices
therefore returns about 5.47 rather than 6, and with 20 choices about
4.87. The bias is depends on both nominal size and the choice
distribution; no single sample size makes it negligible for every
lineup.

With `unbiased = TRUE` the function substitutes the unbiased estimator
of the concentration, \\\sum_i o_i(o_i - 1) / (N(N-1))\\, and returns
its reciprocal, \$\$\hat{E}'\_{\mathrm{corr}} = \frac{N(N-1)}{\sum_i
o_i(o_i - 1)}.\$\$ The reciprocal of an unbiased estimate is not itself
unbiased. For near-fair lineups this can reduce bias, with increased
variability; in concentrated distributions it can overshoot (mean 1.269
in the two-member example). When fewer than two choices are available
the correction is undefined and returns `NA` with a warning. When every
choice is on a different member, the corrected reciprocal is `Inf`,
retained with a warning. If \\N \le k\\, that event can have positive
probability, so the corrected estimator can have an infinite sampling
mean. It is not uniformly superior.

Which to use depends on what is being reported. For a description of the
choice distribution actually observed, the plug-in value is the quantity
of interest. For inference about the lineup that produced those choices,
the framing of Tredoux (1998), the corrected form usually has less bias
when choices are spread across members. Neither is uniformly better.
Note that the other functions in this family
([`esize_T_boot`](https://cgtza2.github.io/r4lineups/reference/esize_T_boot.md),
[`esize_T_ci_n`](https://cgtza2.github.io/r4lineups/reference/esize_T_ci_n.md),
[`i_esize_T`](https://cgtza2.github.io/r4lineups/reference/i_esize_T.md))
use the plug-in form.

## References

Malpass, R. S. (1981). Effective size and defendant bias in eyewitness
identification lineups. *Law and Human Behavior, 5*(4), 299-309.

Agresti, A., & Agresti, B. F. (1978). Statistical analysis of
qualitative variation. *Sociological Methodology, 9*, 204-237.

Malpass, R. S., Tredoux, C., & McQuiston-Surrett, D. (2007). Lineup
construction and lineup fairness. In R. Lindsay, D. F. Ross, J. D. Read,
& M. P. Toglia (Eds.), Handbook of Eyewitness Psychology, Vol. 2: Memory
for people (pp. 155-178). Mahwah, NJ: Lawrence Erlbaum Associates.

Tredoux, C. G. (1998). Statistical inference on measures of lineup
fairness. *Law and Human Behavior, 22(2)*, 217-237.

Tredoux, C. (1999). Statistical considerations when determining measures
of lineup size and lineup bias. *Applied Cognitive Psychology, 13*,
S9-S26.

Wells, G. L., Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
empirically assessing the fairness of a lineup. *Law and Human Behavior,
3*(4), 285-293.

## Examples

``` r
#Data:
lineup_vec <- round(runif(100, 1, 6))
lineup_tab <- table(lineup_vec)

#Call:
e <- esize_T(lineup_tab)
```
