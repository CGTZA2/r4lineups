# Maintainer response to the statistical QA audit (31 October 2026)

This responds to `final_report.md`, `initial_findings.md`, `additional_findings.md` and `repair_history.md`, written by Codex against commit 020e7a2. The repairs were uncommitted in the working tree. Before accepting them, they were reviewed, and four headline defects were reproduced on the pre-repair installed package and confirmed fixed:

- `gen_linevec(table(c(2, 2, 5)), 6)` returned 1 1 2 and now returns 2 2 5.
- A one-column matrix proportion was 1 and is now .25.
- The corrected E′ for all-distinct counts was 6 and is now `Inf`.
- A singleton resample spread over 1:6 and now stays on the observed member.

No existing test was weakened. The changed expectations are stricter or follow an intended behaviour change.

## Decisions

- **Release.** 2.3.0 is released with the repairs. Methods without independent inferential evidence carry their limitations in the help and vignettes, and the outstanding studies form the 2.4 validation plan (`notes/plans/validation_2.4.md`). The audit's HOLD reflects its own protocol, not a known remaining error.
- **pyWitness objective.** The corrected Pearson objective is the default. `control = list(objective = "pywitness")` leaves the engine untouched, so results from pyWitness itself, including published fits, can be reproduced. An opt-in test checks both paths (24 assertions passing).

## Point-by-point

| Finding | Assessment | Disposition |
|---|---|---|
| Independent E comparisons resampled as row pairs | Real, high | Repair accepted |
| Utility reject-all/accept-all on custom grids | Real; extends the 2.3.0 reject-all fix | Accepted |
| ROC synthetic origin; z-ROC dropped zero-count criteria and bootstrap rows | Real | Accepted |
| Grouped calibration/ANRI resolved the design across groups | Real | Accepted |
| Contradictory estimated-mode input accepted | Real | Accepted; now an error |
| z-ROC SD-ratio help reversed; CI level hard-coded | Real, reporting | Accepted |
| Table/matrix conversion changed chosen members | Real, high; reproduced | Accepted |
| Corrected E′ replaced Inf by the plug-in value | Real, high; reproduced | Accepted |
| mSDT log-tail underflow | Real, numerical | Accepted |
| Bins silently omitting observations | Real | Accepted; bins must now cover the data |
| 2-HT rejection gate at 2% | Real, high | Accepted. When no finite fit exists, the error now names the cause, e.g. "Only 1 of 600 witnesses rejected…" |
| Power analysis conditioned away failures | Real | Accepted (`n_failed`, `n_attempted`, warning) |
| `gen_boot_propmean_se` dropped NA silently | Real | Accepted |
| pyWitness rejection-cell denominators reversed | Real by inspection and recomputation | Accepted as default, with the parity option above |
| Log-ratio variance and pAUC "effect size" labels | Real, reporting | Accepted |
| Singleton `sample(x)`; matrix `[[1]]`; fractional k or position | Real | Accepted |
| Plug-in E′ bootstrap cannot reach k | Correct mathematics | Accepted; the plot subtitle and vignette text no longer read E < k as bias |
| Diagnosticity, z-ROC and MAX bootstrap failures not counted | Real | Accepted |
| RAC mean time used correct IDs only | Real | Accepted |
| 2-HT optimiser false failures | Real (an intermediate repair) | Accepted; the feasible inverse is used directly |
| SDT variance recommendation; "undefined" functional size; RAC feedback claim | Real, reporting | Accepted |
| Effective-size workaround in the lineup-models vignette | Partly. Colin's wording ("a workaround") is kept, and the caution that it is not validated is added | Modified |
| `gen_linevec` help example; one-point utility grid; LR = 1 labelled innocence | Real | Accepted |
| Bayesian calibration conditional likelihood | Correct; a sampling-model limitation | Accepted (wording); fixed-arm model listed for 2.4 |
| Statistical limitations 1–9 in the final report | Correct | Stated in the help and vignettes; studies listed for 2.4 |
| Remaining work (Multi-d′ tables, z-ROC/diagnosticity, calibration designs, 2-HT boundaries, pyWitness unequal/correlated, GLMM slopes, power, face/mesh, browser app) | Not errors; evidence gaps | Moved to `notes/plans/validation_2.4.md` |
| Manuscript QA paragraph | Reasonable | Kept, with a clause on the parity option; Colin to review the AI-use disclosure; rebuild the JOSS PDF |

## Draft note to the pyWitness maintainers

> In pyWitness revision e726dcfc0, `ModelFits.calculateChi2` with `chi2Var = "expected"` appears to divide the two rejection cells (target-present and target-absent rejections) by the observed rather than the expected frequency, and the reverse with `chi2Var = "observed"`. For one independently generated dataset, the Ensemble fit reported χ² = 57.511, whereas Σ(O − E)²/E recomputed from the returned cells is 56.715. The fit also leaves its parameters at the optimiser's last evaluation rather than its returned minimum. r4lineups 2.3.0 corrects both for each fit by default and can reproduce the engine unchanged. We would be glad to share the reproduction.
