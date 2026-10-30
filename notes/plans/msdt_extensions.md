# Plan: mSDT extensions (correlated fillers, unequal variance)

Moved out of `vignettes/msdt_model.Rmd` on 30 October 2026 at Colin's request, to be kept as a plan for future work rather than published in the vignette. The relevant functions are in `R/msdt.R` (`dmax_filler()`, `pmax_filler()`, `estimate_msdt_params()`, `plot_msdt_joint()`) and `R/max_sdt_fit.R` (`fit_max_sdt()`).

The functions here implement the **core** mSDT assumptions: equal
variance and independent filler signals. Yang et al. (2025) discuss
extensions that are not yet implemented. This note sets out what each would take.

## 1. Correlated filler signals

**Why it is important.** As the vignette notes, fillers matched to a common
description are unlikely to be independent, and the error runs in an
unsafe direction: assuming independence overstates the competition the
suspect faces.

**How to implement it.** Equicorrelated fillers admit a one-factor
representation. Writing \(\rho\) for the common correlation,

\[
f_i = \sqrt{\rho}\,Z + \sqrt{1-\rho}\,e_i
\]

with \(Z\) and the \(e_i\) independent standard normals. Conditional on
\(Z\) the fillers *are* independent, so the existing power-of-\(\Phi\)
result applies inside an expectation over \(Z\):

\[
P(\max < x) = \int \phi(z)\,
\Phi\!\left(\frac{x - \sqrt{\rho}\,z}{\sqrt{1-\rho}}\right)^{m-1} dz
\]

a one-dimensional integral, evaluable with `stats::integrate()`. This
extends `dmax_filler()` and friends by a single `rho` argument
defaulting to 0, which reduces exactly to the current behaviour.

**Effort:** small. **Risk:** low — the formula is checkable against
simulation, and \(\rho = 0\) must reproduce the existing functions.

## 2. Unequal variances

**Why it is important.** The empirical evidence in recognition memory
generally favours a culprit distribution with larger variance than the
filler distribution, and assuming equality distorts \(d'\).

**The obstacle is identifiability, not arithmetic.** With a culprit
signal \(N(d', \sigma^2)\) the rejection rates become

\[
\Pr(\mathrm{REJ}\mid I) = \Phi(\gamma)^m, \qquad
\Pr(\mathrm{REJ}\mid G) = \Phi\!\left(\frac{\gamma - d'}{\sigma}\right)\Phi(\gamma)^{m-1}
\]

which is **two equations in three unknowns**. \(\sigma\) is simply not
identified from rejection rates, so `estimate_msdt_params()` cannot be
extended to cover it. The extension belongs in `fit_max_sdt()`, which
uses the full response breakdown, or requires confidence data and the
z-ROC slope (`fit_sdt_roc()`).

**Effort:** moderate. **Risk:** higher — the third parameter must be shown
to be recoverable before the option is offered, which means a simulation
study sweeping \(\sigma\) and sample size and checking recovery, on the
model of the identifiability concerns raised in
`vignette("lineup_memory_models")`.

## 3. Order of work

Correlated fillers first: it is self-contained, verifiable against
simulation, and backwards compatible. Unequal variance second, and only
after a recovery study establishes that the parameter can be estimated at
realistic sample sizes. Shipping an option that returns an unidentified
parameter would be worse than not shipping it.
