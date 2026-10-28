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
substituted for the unknown \\p_i\\. Plug-in estimators of \\\sum_i
p_i^2\\ are biased upward, so \\E'\\ is biased **downward**. For \\N\\
choices spread evenly over \\k\\ members the bias is approximately
\$\$E\[\hat{E}'\] \approx \frac{Nk}{N + k - 1}, \qquad \mathrm{bias} =
-\frac{k(k-1)}{N + k - 1}.\$\$ A perfectly fair six-person lineup
assessed with 50 choices therefore returns about 5.47 rather than 6, and
with 20 choices about 4.87. The bias is negligible above a few hundred
choices.

With `unbiased = TRUE` the function substitutes the unbiased estimator
of \\\sum_i p_i^2\\, giving \$\$\hat{E}'\_{\mathrm{corr}} =
\frac{N(N-1)}{\sum_i o_i(o_i - 1)}.\$\$ In simulation this removes
essentially all of the bias, at a modest increase in variance that
disappears by \\N \approx 100\\.

Which to use depends on what is being reported. For a description of the
choice distribution actually observed, the plug-in value is the quantity
of interest. For inference about the lineup that produced those choices
— which is the framing of Tredoux (1998) — the corrected form is the
better estimator. Note that the other functions in this family
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
