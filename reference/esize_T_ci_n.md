# Effective Size with Confidence Intervals from Normal Theory (Tredoux, 1998)

Computes effective size E' (Tredoux, 1998) together with a confidence
interval derived from the normal-theory variance formula in Tredoux
(1998).

## Usage

``` r
esize_T_ci_n(lineup_tab, alpha)
```

## Arguments

- lineup_tab:

  A table of lineup choices.

- alpha:

  Confidence level as a decimal (e.g., 0.95 for a 95-percent CI).

## Details

The variance of the diversity index I is estimated via Tredoux's (1998)
closed-form expression: \$\$\mathrm{Var}(I) \approx
\frac{4}{N}\left(\sum_i p_i^3 - \left(\sum_i p_i^2\right)^2\right).\$\$
Normal quantiles are applied to I and the resulting bounds are
back-transformed to the E' scale via \\E' = 1/(1-I)\\. For small samples
or skewed distributions, all interval methods require qualification. In
particular, plug-in bootstrap intervals also fail at exact uniform
fairness; see
[`esize_boot_dist`](https://cgtza2.github.io/r4lineups/reference/esize_boot_dist.md).
Bayesian credible intervals describe a stated posterior rather than
guaranteeing fixed-parameter frequentist coverage.

Note that this concerns the *interval*. The point estimate itself is
also biased downward near uniform choice distributions at small samples,
although the direction is not universal, since \\E'\\ is a plug-in
estimator; see the “Small-sample bias” section of
[`esize_T`](https://cgtza2.github.io/r4lineups/reference/esize_T.md).
This function uses the plug-in form. This first-order interval is
unreliable in small samples and at uniform or concentrated boundary
distributions. A zero estimated variance yields a degenerate interval,
not evidence of zero population uncertainty. An ordinary bootstrap is
also not a general solution to these boundary problems.

## References

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

## Examples

``` r
#Data:
lineup_vec <- round(runif(100, 1, 6))
lineup_tab <- table(lineup_vec)

#Call:
e_ci <- esize_T_ci_n(lineup_tab, .95)
```
