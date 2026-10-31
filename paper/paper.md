---
title: "r4lineups: Statistical Inference on Lineup Fairness"
tags:
  - R
  - eyewitness identification
  - lineup fairness
  - signal detection theory
  - forensic psychology
authors:
  - name: Colin G. Tredoux
    affiliation: 1
    corresponding: true
  - name: Tamsyn M. Naylor
    affiliation: 1
affiliations:
  - name: Department of Psychology, University of Cape Town, South Africa
    index: 1
date: 29 October 2026
bibliography: paper.bib
---

# Summary

In eyewitness lineups, a police officer asks a witness to decide whether a
person seen during an event is present among a set of alternatives, and to
identify that person if present. The evidential value of a lineup
depends not only on whether a suspect is chosen, but also on whether the
suspect stands out, whether the fillers are plausible alternatives, and how
accuracy changes with confidence and response time. These questions have
generated distinct traditions of lineup-fairness measurement, confidence-based
analysis, and models of lineup decisions [@wells1979; @wells1980;
@malpass1981; @tredoux1998; @mickes2012].

`r4lineups` is an R package that brings those traditions into one collection.
It provides classical and modern measures of lineup fairness,
bootstrap and Bayesian uncertainty summaries, ROC/CAC/RAC and full-ROC
analyses, calibration and evidential measures, signal-detection and
two-high-threshold models, Lee and Penrod's Multi-d′ framework
[@lee2019], simulation and power tools, and optional face-similarity and
lineup-memory-model workflows. An interactive application covers most of
these analyses and runs in a web browser without installation. The intended users are
eyewitness researchers, forensic psychologists, students, and applied analysts.

# Statement of need

Mock-witness assessments and eyewitness experiments produce different but
related data. Mock witnesses who did not see the perpetrator choose the lineup
member who best matches a description; uneven choices indicate that some
members are more plausible than others. Both lineup bias and lineup effective
size are traditionally computed from mock-witness choices. Eyewitness
experiments instead compare
target-present and target-absent decisions, often with confidence and response
time, although eyewitness data can also support estimates of lineup fairness
[@fitzgerald2023]. Lee and Penrod, for example, proposed the difference
between the z-transformed rates at which witnesses choose the innocent suspect
and the fillers from target-absent lineups as a resultant index of lineup bias
[@lee2019]. Analysts therefore need both lineup-construction
measures--including
suspect-selection proportion, foil bias, functional size, Malpass effective
size, and Tredoux effective size--and measures of identification performance
and evidential value [@doob1973; @wells1979; @malpass1981; @tredoux1998].

Before `r4lineups`, these computations were commonly distributed across
article supplements, small bespoke scripts, and general statistical packages.
That fragmentation made input conventions, denominators, base-rate
assumptions, continuity corrections, and uncertainty procedures difficult to
audit consistently. `r4lineups` supplies lineup-specific validation and common
data conventions together with worked examples. Version 2.3.0 is a major
extension of the original package, first released on CRAN as version 0.1.1 in
2018 [@r4lineups].

# State of the field

General R tools such as `boot` [@boot] provide important computational
infrastructure, but do not encode eyewitness response categories or estimands.
The R
package `fullROC` focuses on full receiver operating characteristic curves
using suspect, filler, and rejection responses [@yang2023]. The Python package
`pyWitness` provides a broad eyewitness-analysis toolkit, including
confidence-based signal-detection models [@mickes2024].

`r4lineups` occupies a complementary position. It combines mock-witness
fairness inference, confidence-based evidential analysis, Bayesian summaries,
model-based analyses, simulation, and reporting in an R-native interface. It
does not duplicate the established Python likelihood/model-fitting engine.
Instead, its optional `fit_lineup_models()` bridge calls a pinned pyWitness
revision and returns R-native objects. Independent Observations,
Ensemble, and Integration models can therefore be examined from R while
remaining comparable with the reference implementation [@wixted2018]. The
older `fit_max_sdt()` function, also available in `r4lineups`, is explicitly
narrower: it is a single-criterion,
aggregate, equal-variance Independent Observations fit, not the complete
confidence-based Wixted model.

# Software design

The core design separates two input families. Fairness functions accept lineup
choice vectors or tables. Confidence-based functions use trial-level data with
`target_present`, `identification`, and `confidence`, with optional fields such
as response time and experimental condition. Analysis wrappers return
structured S3 objects with `print()`, `summary()`, and `plot()` methods, making
assumptions and derived quantities inspectable while supporting normal R
workflows. Two helpers make data coding explicit: `lineup_table()` tabulates
choices over every lineup position, so that members nobody chose remain
visible, and `classify_lineup_choices()` derives suspect, filler, and rejection
responses from the chosen position and the suspect's position.

The package favours R-native calculations for its statistical core and makes
external systems explicit. Face embeddings and the pyWitness bridge require
separately managed Python environments; neither is initialised or installed
when the package loads or during CRAN checks. Results from the lineup-model
bridge contain extracted R tables rather than live Python objects, so saved
analyses remain readable without an active Python session. This design trades
some installation convenience for reproducibility and a clear
boundary between package code and external models.

We built the interactive application from modules that call only exported
package functions, so that every number it reports can be reproduced in R.
Confidence-based data are loaded once, with the nominal lineup size and any
grouping variable used for comparisons, and bootstrap and posterior analyses
run on request with a user-supplied seed. The application is installed with
the package and also runs entirely in the browser through WebAssembly, from
the package website. In the browser version, mock-witness and experimental
data stay on the user's computer, an important safeguard when the data come
from police casework.

The sixteen vignettes carry the methodological detail that users will need.
They cover fairness and diagnosticity, Bayesian inference,
ROC/CAC/RAC and full ROC, calibration and decision measures, expected
information gain and error-rate ranges, Winter's two-high-threshold model,
summary and regression-based signal-detection methods, the Multi-d′, mSDT and
Wixted models, simulation and power, and optional face similarity.
Validation of selected methods combines published definitions, independent
hand calculations, invariance identities, numerical integration and simulation
recovery. The lineup-memory bridge also checks its interface against a pinned
pyWitness revision [@mickes2024]. An independent statistical audit records the conditions
examined, evidence gaps and limitations of approximate inference; selected
counterexamples are retained as regression tests. The pyWitness bridge applies
a per-fit correction to the pinned engine's rejection-cell variance denominators
so its default objective is consistently Pearson chi-squared, without modifying
the installed Python environment; an option reproduces the engine's own
objective for comparison with published fits.

# Research impact statement

`r4lineups` has been publicly available through CRAN since 2018 and has been
used in published eyewitness research, including analyses of eyewitness
decision processes [@mansour2024; @mansour2025]. Version 2.3.0 substantially
extends that established research tool while preserving the classical
fairness functions used by earlier work. Its immediate scholarly contribution
is a tested R workflow spanning lineup construction, identification outcomes,
confidence, evidence, and competing process models. The package website,
sixteen vignettes, example datasets, and the browser-based
application also support teaching, method comparison, and applied review.

# AI usage disclosure

Development used multiple dynamically updated frontier models: Anthropic
Claude Sonnet 4 and 5 and Claude Opus 4.6, 4.8 and 5.5, and OpenAI Codex with
GPT-5-family models including GPT-5.5 and GPT-5.6 Sol. Additional Claude and
Codex sessions were used, but exact session-level model identifiers were not
consistently recorded. These tools assisted with code review and drafting,
debugging, test scaffolding, documentation, package-check remediation, the
interactive application, and manuscript editing. Colin Tredoux and Tamsyn Naylor reviewed, modified, and
validated the AI-assisted outputs, made the statistical and software-design
decisions, and retain full responsibility for accuracy, originality,
licensing, and scholarly claims.

# Acknowledgements

We gratefully acknowledge the many researchers whose publications provide the
computational foundations implemented in `r4lineups`, including work on lineup
fairness, confidence-based analysis, evidential measures, and models of lineup
memory [@smith2020; @starns2023; @fitzgerald2023; @winter2022; @wixted2018].
Errors in translating that published work into package code remain the
responsibility of the package authors and may be reported through the
[GitHub issue tracker](https://github.com/CGTZA2/r4lineups/issues).

This work was supported in part by incentive grants from the National Research
Foundation of South Africa awarded to Colin G. Tredoux during 2016--2026
(maximum value approximately US$2,000). The funder had no role in the
software's design, development, validation, manuscript preparation, or the
decision to submit. The authors declare no conflicts of interest.

# References
