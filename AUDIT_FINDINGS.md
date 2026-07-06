# r4lineups — CRAN/JOSS Audit Findings

Scratch log of the component-by-component review. Severity: **P0**
(blocker / wrong results), **P1** (should fix before submission), **P2**
(polish / nice-to-have). This file is build-ignored (add to
`.Rbuildignore` in Track B).

------------------------------------------------------------------------

## Component 1 — mSDT (`R/msdt.R`, `R/max_sdt_fit.R`)

**Status: numerically verified correct.** No P0/P1 issues in `msdt.R`.

Verification run (sourced `R/msdt.R`, lineup size 6): - CDF identity
`pmax_filler(q) == pnorm(q)^(m-1)` ✓ - Quantile round-trip
`pmax_filler(qmax_filler(p)) == p` ✓ - `lower.tail=FALSE` consistent
with `1-p` ✓ - `max_filler_moments(6)$mean = 1.162964` matches known
E\[max of 5 N(0,1)\] = 1.16296; Monte-Carlo mean 1.1613; var 0.4475,
skewness +0.303 (correct sign/scale) ✓ - Parameter recovery exact:
γ=0.5, d′=1.2 → recovered γ=0.5, d′=1.2 ✓ - `rmax_filler` sample mean
1.1622 vs theory 1.1630 ✓

Docs accuracy: vignette `msdt_model.Rmd` formulas `Pr(REJ|I)=Φ(γ)^m`,
`Pr(REJ|G)=Φ(γ−d′)Φ(γ)^{m−1}` match the implementation exactly; scope
caveats (equal variance, independent fillers) honestly stated. Reference
Yang, Burke & Healy (2025) present.

Minor (P2): - `pmax_filler(log.p=TRUE)` and `dmax_filler(log=TRUE)` can
return `-Inf` at the extreme lower tail (`log(0)`); acceptable, but
could guard for tidiness. - `R/max_sdt_fit.R` (the compound-decision MAX
SDT fitter, commit 023d004) not yet reviewed in this pass — review with
model-comparison or as its own mini-pass.

### `R/max_sdt_fit.R` (compound-decision MAX SDT fitter) — reviewed + FIXED

> **FIXED (this session).** Applied: (1) deterministic multistart grid
> (finer on λ) + Nelder-Mead polish replacing the single boundary-prone
> start; (2) proper partition cells `{correct-ID, filler-ID, reject}`
> replacing the overlapping `{hit, choose}` cells; (3) floored expected
> counts (`pmax(pred,1e-8)`) replacing the `pred>0.5` cell-exclusion
> that created the plateau; (4) corrected GoF df =
> `3*n_conditions − np`; (5) honest convergence reporting (no forced
> `0L`); same fix in the bootstrap helper `.max_sdt_make_obj`. Verified:
> documented example now d′=1.94, λ=1.15, χ²(1)=4.41; exact parameter
> recovery for three (d′,λ) pairs; two-condition df 2/3; reproducible
> bootstrap CIs. Updated two tests in `test-max-sdt-fit.R` that asserted
> the old df (3→1, 6→2). All 30 tests pass. Original analysis retained
> below for the record.

Model probabilities verified correct:
`hit_prob = ∫_λ^∞ φ(x−d)Φ(x)^{n−1}dx`, `tpc_prob = 1−Φ(λ−d)Φ(λ)^{n−1}`,
`fa_prob = 1−Φ(λ)^n`. This is the full-information fitter (uses all five
outcome counts), the right tool when choice data are available.

**P0 — optimizer returns a non-global boundary estimate.** The
documented headline example
(`n_hit=69,n_tp_choose=82,n_fa=64,N_tp=96,N_ta=106`) returns d′=1.85,
λ=−5 (pinned at lower bound), χ²=18.68, predicting everyone chooses
(reject cells→0). Profiling the *same* objective shows a much better fit
at λ≈1.0, d′≈3.2, χ²≈8.3; the proper-partition objective minimises at
λ≈1.1, d′≈1.9, χ²≈4.6 (would pass GoF). Root causes: (1) flat plateau at
low λ where reject predictions ≈0 are dropped by the `pred > 0.5` cell
filter; (2) χ² spike at λ=0; (3) single start `(1,0)` + boundary-prone
L-BFGS-B, no multistart. Net effect: reported d′ can be badly wrong.
Fix: add multistart over a λ grid (e.g. −2..2) and/or a robust
objective; reconsider the `pred>0.5` exclusion that flattens the
surface; widen/relax bounds or penalise the boundary.

**P1 — GoF degrees of freedom overstated.**
`df = n_cells − n_params = 5 − 2 = 3` (one condition) ignores the two
fixed row totals (`choose+reject=N_tp`, `fa+reject=N_ta`). Correct df =
(3-cat TP → 2) + (2-cat TA → 1) − 2 params = **1**. Two-condition case:
code gives 10−4=6, correct is 2. p-values too lenient.
(`compare_max_sdt` Δdf is correct.)

**P2 — overlapping cells + convergence masking.** χ² objective uses
`{n_hit, n_tp_choose}` with `n_hit ⊂ n_tp_choose` (double-counts
choosers); cleaner partition is `{correct-ID, filler-ID, reject}`. The
bootstrap simulator (L346-348) *does* nest hits correctly, so objective
and bootstrap DGP are inconsistent. Also `opt$convergence <- 0L` forced
after Nelder-Mead fallback
([max_sdt_fit.R:284](https://cgtza2.github.io/r4lineups/R/max_sdt_fit.R#L284))
masks non-convergence — same pattern flagged for Winter 2-HT.

**P1 — documented example is a poor showcase**: it produces the
boundary/degenerate fit above. Replace with data that yield an interior,
well-fitting solution once the optimizer is fixed.

Documentation gap (P1, no code bug): -
[`estimate_msdt_params()`](https://cgtza2.github.io/r4lineups/reference/estimate_msdt_params.md)
is a **method-of-moments estimator** that is exactly identified from the
two rejection rates (Pr(REJ\|I) fixes γ; the CA→CP rejection gap fixes
d′). It is *information-lossy* — it ignores choice composition
(suspect-ID vs filler-ID) and the filler-choice spread, which are the
most diagnostic outcomes (cf. Sporer on choosers vs non-choosers). The
`.Rd` and vignette should state this explicitly and point users with
full response breakdowns to the likelihood-based fitter
(`max_sdt_fit.R`) / the 2-HT model. Frame for the JOSS paper the same
way so a reviewer doesn’t read rejection-only estimation as the
recommended mSDT fit.

------------------------------------------------------------------------

## Component 2 — Full ROC (`R/fullroc_functions.R`) — reviewed

Method verified against benchmarks (`make_fullroc_data` only): -
Identical TP/TA response distributions → AUC = 0.527 (≈0.5; curve ends
exactly at (1,1)) ✓ — see bias note below. - Perfect separation
(TP→suspect, TA→reject) → AUC = 1.000 ✓ - Realistic (TP suspect-heavy) →
AUC 0.78 (diagnosticity) / 0.77 (apriori) ✓ Construction correct: all
responses crossed with confidence, each category gets HR=n/n_tp and
FAR=n/n_ta, ordered by DR (descending) or a-priori, cumulated to span
(0,0)→(1,1). Epsilon affects only *ordering* (DR when FAR=0), not curve
coordinates; empty cells dropped; trapezoidal AUC correct.

**P2 — `order="apriori"` + `conf_bins` raises “NAs introduced by
coercion”. \[FIXED\]** The a-priori confidence tiebreak did
`as.numeric(conf_bin)`; with `conf_bins` the labels are `"conf_1"`, … →
`NA`, silently breaking within-decision ordering. Fixed by sorting on
the bin index `as.integer(factor(conf_bin, levels = conf_levels))`.
Verified: no warning, AUC 0.77, order now
suspect(hi→lo)→filler(lo→hi)→reject(lo→hi), curve ends at (1,1); default
ordering unchanged (regression clean).

**P1 (doc, not a bug) — diagnosticity ordering is optimistically
biased.** Sorting cells by *sample* DR bows the curve upward from noise
alone: identical TP/TA → AUC 0.527, not 0.500; bias grows with the
number of (decision×conf) cells, shrinks with N. This is why Smith &
Yang offer a-priori ordering. `.Rd`/vignette should warn
diagnosticity-ordered AUC is not unbiased and recommend a-priori
ordering (or a permutation baseline) for inference.

**P2 — ggplot2 deprecation in `plot_fullroc`. \[PARTLY FIXED\]**
`geom_line(size=1)` → `linewidth` fixed. **Track B sweep still needed**:
grep the whole package for `geom_line(size=` / `geom_*(... size=`
line-width uses and convert to `linewidth`.

**P1 (CRAN) — `\dontrun{}` unnecessary.**
`make_fullroc_data`/`make_fullroc` examples use only base-R simulated
data → make runnable or `\donttest`. Part of package-wide cleanup.

**P2 — defensive idioms.** `1:nrow(...)` / `2:nrow(...)` →
`seq_len`/`seq_along`.

------------------------------------------------------------------------

## Component 3 — EIG & PPV range (`eig_functions.R`, `ppv_range_functions.R`) — reviewed

**EIG verified correct.** `EIG = Σ_x p(x)[H(prior) − H(posterior(x))]` =
mutual information I(guilt;response). Checks: uninformative responses →
EIG ≈ 0.0007 bits; perfect separation at prior 0.5 → EIG = 1.000 bit =
H(0.5). Bayes posterior + total-probability weighting all correct.

**P1 — duplicate
[`entropy()`](https://cgtza2.github.io/r4lineups/reference/entropy.md)
definition. \[FIXED\]** Defined twice (both internal, not exported):
`eig_functions.R` `entropy(p, base=2)` and `bayes_curves_functions.R`
`entropy(p)`. Whichever sourced last in collation won package-wide; only
alphabetical luck (eig after bayes) kept it working, and roxygen merged
both into one `entropy.Rd` with a duplicated `\usage` line. Fixed by
deleting the `bayes_curves` duplicate; the four call sites (2 per file)
now resolve to the single `entropy(p, base=2)`. Verified both call
styles return 1.0. (Track B: re-run `document()` to clean
`man/entropy.Rd`.)

**P1 — `ppv_by_confidence(correction="effective")` computed the wrong
effective size. \[FIXED\]** The no-member-data fallback called
`esize_T(table(pseudo_table))`, which tabulated the *values* of the
per-position count vector, collapsing E′ to a near-constant ~1.38
regardless of data — so the “effective” correction silently behaved like
“none”. Fixed to `esize_T(pseudo_table)`. Verified: E′ now ~5.7 (≈
nominal 6 for an even lineup), and `effective` PPV (0.879) sits between
`none` (0.559) and `nominal` (0.884). (Member-level branch
`esize_T(table(ta_conf$lineup_member))` was already correct.)

**P2 (doc) — PPV base rate is implicit.**
`PPV = n_guilty / (n_guilty + n_innocent_est)` mixes raw TP and TA
counts, so the prior/base rate is implicitly the experiment’s TP:TA
ratio. No `base_rate` argument is exposed. Document this, or add a
base-rate parameter, so users don’t misread PPV as base-rate-free. The
pseudo-distribution fallback (even filler spread) is also a modeling
approximation worth stating.

**P1 (CRAN) — `\dontrun{}` on runnable examples**: `compute_eig`,
`make_eig`, `ppv_by_confidence`, `ppv_range_by_confidence` examples are
pure base-R/sim data → make runnable or `\donttest`. Part of
package-wide cleanup.

------------------------------------------------------------------------

## Component 4 — Calibration & ANRI (`calibration_functions.R`, `anri_functions.R`) — reviewed

**Math verified correct.** `C = Σ(n_j/N)(c_j−a_j)²`, `O/U = c̄−ā`,
`NRI = [Σ(n_j/N)(a_j−ā)²]/[ā(1−ā)]`, `ANRI = (N·NRI−J+1)/(N−J+1)`.
Checks: good resolution → NRI 0.234 / ANRI 0.232; flat accuracy → NRI 0
/ ANRI −0.0017 (correct small-sample bias correction, can go slightly
negative); well-calibrated data → C=0, O/U=0; ANRI formula hand-check
exact. Bootstrap CI and `compare_anri`
(difference-of-independent-bootstraps, percentile CI, CI-excludes-0
test) are sound.

**P2 — confidence scale detection is a silent foot-gun.**
`make_calibration_data` infers the scale by
`if (max(confidence) > 1) divide by 100`. A Likert 1–7 (or 0–10) scale
therefore gets divided by 100 → `overall_confidence ≈ 0.04`, making C
and O/U meaningless, with **no warning**. Verified. Fix options: add an
explicit `confidence_scale = c("auto","0-1","0-100")` arg, and/or warn
when `1 < max(conf) < ~20` (ambiguous). Touches `make_calibration_data`
and its callers (`compute_anri`, `make_calibration_by_condition`) —
handle as a small coordinated change.

**P2 — `J` varies across bootstrap resamples.** `compute_anri` sets
`J = nrow(non-empty bins)`; a resample with an empty bin shrinks `J`,
perturbing the `N−J+1` correction. Minor; consider fixing
`J = length(confidence_bins) − 1` for stability, or document.

**P3 — ANRI can be negative** (by construction, like adjusted R²) — note
in `.Rd`. **P3 — redundant `set.seed(seed)`** at top of `compare_anri`
(inner `bootstrap_anri` calls reseed anyway).

No `\dontrun`/example issues here (these functions ship without
`@examples`; consider *adding* runnable examples for CRAN/JOSS
completeness).

------------------------------------------------------------------------

## Component 5 — Bayesian curves & utility (`bayes_curves_functions.R`, `utility_functions.R`, `dpp_functions.R`) — reviewed

**All four sub-parts verified correct; no code bugs.** - **Bayes
prior-posterior curves** (`make_bayes_curves`): posterior at prior 0.5
matches hand Bayes (0.8257); `info_gain = H(prior) − H(posterior)` uses
the now-single
[`entropy()`](https://cgtza2.github.io/r4lineups/reference/entropy.md).
✓ - **DPP** (`make_dpp`, Smith et al. 2018):
`DPP = 1 − AUC_obs/AUC_perfect`, perfect ROC = rectangle area `max_fa`.
Perfect data → DPP 0; realistic → 0.72. `make_rocdata` adds the (0,0)
origin so the observed/perfect integration domains both span \[0,
max_fa\]. ✓ - **Expected utility** (`make_utility_curves`, Lampinen et
al. 2019): EU decomposition matches hand calc; max-utility criterion
selected correctly. ✓ - **BREE** (`make_bree_curve`, in
`bayes_curves_functions.R` — *not* utility_functions.R as an earlier
inventory said): prior-inversion algebra correct — self-comparison gives
delta≡0; cross-comparison yields the prior_b whose posterior equals A’s.
✓

**P2 (doc/method) — utility FA-rate hybrid.** `make_utility_curves`
estimates innocent-suspect FAs as
`n_false_suspects + n_false_fillers/lineup_size`. With a designated
innocent suspect this *double counts* (direct innocent-suspect picks + a
filler-derived estimate); it is only strictly right for
no-designated-suspect designs. Same modeling issue as PPV — document the
assumption (and ideally reconcile the two so the package is consistent).

**P3 (perf) — `make_utility_difference`/`compare_utility` recompute
`make_utility_curves` from raw data for all 99 base rates**, though
per-criterion hit/FA rates don’t depend on base rate (only the EU
weighting/argmax do). Compute rates once, reweight per base rate.

**P1 (CRAN) — `\dontrun{}`** on `compute_dpp`, `compare_utility` (and
others) — pure sim data, make runnable/`\donttest`.

------------------------------------------------------------------------

## Component 6 — Winter 2-HT (`winter_2ht.R`, `winter_2ht_boot.R`, `winter_2ht_plots.R`) — reviewed

Model equations verified: both TP and TA probability vectors sum to 1;
the MPT tree (detect→correct; else biased/guess/reject) matches Winter
et al. (2022). Multinomial MLE. Parameter recovery excellent: true
(dP,dA,b,g)=(.45,.30,.08,.55) → est (.454,.293,.073,.552).

**P1 — the goodness-of-fit test in `summary.winter_2ht` is statistically
invalid (model is SATURATED).** With one TP/TA condition there are only
4 independent data points (2 per 3-cell multinomial given fixed totals)
and 4 free parameters → **0 residual df**. Verified: fitted expected ≡
observed (147→147, 94→94.16, …), χ²≈0.0008. But
[`summary()`](https://rdrr.io/r/base/summary.html) computes
`df <- 6 - 4 = 2` and a p-value, which will *always* show near-perfect
fit (χ²≈0, p≈1) — false reassurance. A single-condition 2-HT model
cannot be tested for absolute fit; Winter et al. test via multiple
conditions / parameter constraints. Fix: drop the single-condition χ²
test (or replace with an explicit “saturated, df=0” note; keep
observed-vs-expected as an optimisation sanity check). **Paper impact:
do not report a 2-HT GoF χ²(2) for single-condition fits.**

**Correction to prior plan note — NO convergence masking here.** The
plan flagged `fit_winter_2ht` ~L280-285 for forced `convergence<-0`;
misattribution (those lines are `.extract_counts_from_df`).
`fit_winter_2ht` reports `optim` convergence honestly; print/ summary
warn on non-zero. The masking bug was in `max_sdt_fit.R` (already
fixed).

**P2 — single start, boundary solutions.** One L-BFGS-B start from
(0.3,0.1,0.05,0.5); the example drove `dA→0` (boundary), where Hessian
SEs are unreliable. MLE generically unique for a saturated model, so low
priority — a small multistart + boundary caveat on SEs would harden it.

**P1 (CRAN) — `\dontrun{}`** on `fit_winter_2ht` example (count-data
path is pure base R).

> **FIXED (this session):** `summary.winter_2ht` now prints a “Model
> identification: Saturated (just-identified), df=0” note plus the
> residual obs-vs-expected discrepancy as a convergence check, replacing
> the invalid χ²(2) test. Verified.

------------------------------------------------------------------------

## Track B — applied this session

- **Calibration scale foot-gun \[FIXED\].** Added
  `confidence_scale = c("auto","0-1","0-100")` to
  `make_calibration_data` and threaded it through `make_calibration`,
  `make_calibration_by_condition`, `calibration_bayes`, `compute_anri`,
  `bootstrap_anri`, `compare_anri`. `auto` stays backward-compatible
  (0-100 data unchanged) but now *warns* when the max looks like a
  Likert/0-10 scale (≤ 20). Roxygen `@param` added to all 7. Verified:
  0-100 silent, Likert-1-7 warns, explicit `"0-1"` honored, ANRI
  unaffected.

### Outstanding for Track B (need full toolchain in your env)

- **Run `devtools::document()`** to regenerate `man/*.Rd` + `NAMESPACE`
  for the new `confidence_scale` args and the deduped
  [`entropy()`](https://cgtza2.github.io/r4lineups/reference/entropy.md).
  Note: regeneration ALSO clears pre-existing drift roxygen detected
  (missing `importFrom(stats, coef/lm/fitted/density/ residuals)`, a
  missing `gamma` @param) — these were already stale in the committed
  tree, independent of this session.
- `\dontrun{}` → `\donttest{}`/runnable sweep; `T`/`F` → `TRUE`/`FALSE`;
  create `cran-comments.md`; ggplot `size=`→`linewidth` sweep; add tests
  for Full ROC / EIG-PPV / Winter 2-HT; then `R CMD check --as-cran`.
- JOSS: `paper.md` + `paper.bib`, CI workflow, `CONTRIBUTING.md`.

> **NOTE — concurrent working-tree changes (not from this session):**
> `R/msdt.R`, `R/landmarks.R`, `R/setup.R`, `R/utils.R`, `R/zroc_sdt.R`,
> `R/zzz.R`, and a new `tests/testthat/test-formula-consistency.R` were
> already modified/added in the tree (roxygen `@param` additions etc.)
> by separate work. Left untouched. Reconcile before committing.
