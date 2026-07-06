# SDT via GLM/GLMM

## Overview

This vignette shows how to estimate signal‑detection metrics from
**old/new recognition** data using GLM/GLMM methods, following the
approach described in Wright, Horry, & Skagerberg (2009).

Key idea:

- Encode item status (`is_old`) and response (`said_old`) as 0/1.
- Fit a **probit** or **logit** GLM/GLMM.
- The coefficient on `is_old` corresponds to **d′** (probit) or **lnOR**
  (logit).
- With centered coding (`is_old` → -0.5/+0.5), the intercept corresponds
  to the **criterion**.

## Simulated data

``` r

library(r4lineups)

set.seed(123)
n_subj <- 40
n_items <- 60
df <- data.frame(
  subject_id = rep(1:n_subj, each = n_items),
  item_id = rep(1:n_items, times = n_subj)
)
df$is_old <- rbinom(nrow(df), 1, 0.5)

# true d' around 1.2 with mild subject variability
subj_shift <- rnorm(n_subj, 0, 0.2)
linpred <- (df$is_old * 1.2) + subj_shift[df$subject_id]
df$said_old <- rbinom(nrow(df), 1, stats::pnorm(linpred))
```

## Single‑level GLM (probit)

``` r

glm_fit <- fit_sdt_glm(df, is_old = "is_old", said_old = "said_old", link = "probit")
extract_sdt_metrics(glm_fit)
#> $metric
#> [1] "dprime"
#> 
#> $estimate
#> [1] 1.19898
#> 
#> $criterion
#> [1] -0.5281364
#> 
#> $link
#> [1] "probit"
#> 
#> $centered
#> [1] TRUE
```

## Multilevel GLMM (random intercepts)

``` r

glmm_fit <- fit_sdt_glmm(df,
                         is_old = "is_old",
                         said_old = "said_old",
                         subject_id = "subject_id",
                         item_id = "item_id",
                         link = "probit",
                         random_slope = FALSE)
extract_sdt_metrics(glmm_fit)
#> $metric
#> [1] "dprime"
#> 
#> $estimate
#> [1] 1.219388
#> 
#> $criterion
#> [1] -0.5420796
#> 
#> $link
#> [1] "probit"
#> 
#> $centered
#> [1] TRUE
```

## Including covariates (optional)

``` r

df$delay <- rnorm(nrow(df), 0, 1)
glm_fit_cov <- fit_sdt_glm(df,
                           is_old = "is_old",
                           said_old = "said_old",
                           covariates = "delay",
                           link = "probit",
                           interactions = TRUE)
summary(glm_fit_cov)
#> 
#> Call:
#> stats::glm(formula = form, family = stats::binomial(link = link), 
#>     data = df)
#> 
#> Coefficients:
#>                  Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)      0.528350   0.029288  18.040   <2e-16 ***
#> .is_old_c        1.199197   0.058576  20.473   <2e-16 ***
#> delay           -0.009941   0.029444  -0.338    0.736    
#> .is_old_c:delay  0.049162   0.058888   0.835    0.404    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 3048.3  on 2399  degrees of freedom
#> Residual deviance: 2592.8  on 2396  degrees of freedom
#> AIC: 2600.8
#> 
#> Number of Fisher Scoring iterations: 4
```

## Notes

- **Probit link** ⇒ coefficient on `is_old` = d′.  
- **Logit link** ⇒ coefficient on `is_old` = ln(OR).  
- Criterion is returned only when `is_old` is centered (default).

For multilevel models,
[`fit_sdt_glmm()`](https://cgtza2.github.io/r4lineups/reference/fit_sdt_glmm.md)
requires **lme4**.
