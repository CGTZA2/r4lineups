# Fitzgerald, Tredoux & Juncu (2023) — paper summary

**Citation**: Fitzgerald, R. J., Tredoux, C. G., & Juncu, S. (2023). *Estimation of Eyewitness Error Rates in Fair and Biased Lineups*. Law and Human Behavior.

## One‑paragraph overview
The paper addresses how to estimate the risk that an **innocent suspect** is misidentified in a lineup when lineup fairness varies. Traditional estimates divide the overall mistaken‑ID rate by the **nominal lineup size**, which assumes that all lineup members are equally plausible. Fitzgerald et al. compare that approach to an **effective size correction** that uses the distribution of mistaken IDs across lineup members to estimate the number of *plausible* members. They show that nominal correction is insensitive to lineup bias, whereas effective size correction increases estimated innocent‑suspect risk as bias increases. The authors recommend reporting multiple estimates (nominal, effective, and uncorrected) and conceptualize the resulting **PPV range** as reflecting the continuum of lineup fairness conditions.

## Core contributions
- **Problem**: Innocent‑suspect error rates are often underestimated when lineups contain implausible fillers.
- **Approach**: Compare three ways to estimate innocent‑suspect misidentification risk:
  1) **Nominal size correction** (assumes perfectly fair lineups)
  2) **Effective size correction** (accounts for plausibility; sensitive to bias)
  3) **No correction** (treats all mistaken IDs as innocent‑suspect IDs)
- **Key claim**: Effective size correction tracks lineup bias and should be reported alongside nominal correction, not replaced by it.

## Concepts and definitions
- **Nominal size correction**: `innocent_id_rate = overall_error_rate / lineup_size`.
- **Effective size correction**: replace `lineup_size` with **effective size** computed from the distribution of mistaken IDs across lineup members (Tredoux, 1998).
- **No correction**: `innocent_id_rate = overall_error_rate`.
- **PPV (positive predictive value)**: probability suspect is guilty given an ID. In lineup terms, PPV depends on how innocent‑suspect IDs are estimated; the three correction methods define a **PPV range** from best‑case to worst‑case.

## Effective size logic (Tredoux, 1998)
Effective size reflects the number of *plausible* lineup members, not the nominal count. When mistaken IDs are concentrated on a few members, effective size drops below nominal size, indicating bias.

## Main empirical findings (high‑level)
- Lineup bias increases **designated innocent‑suspect IDs**.
- **Effective size‑corrected** innocent‑suspect estimates increase with bias; **nominal correction** does not.
- Real datasets show effective size is often < nominal size, implying that nominal correction can underestimate error rates.
- PPV estimates vary substantially depending on the correction method; the authors recommend reporting the **range** rather than a single point.

## Practical recommendations
- Report **both nominal and effective size corrections**, and optionally the uncorrected estimate, to reflect the range of plausible investigative scenarios.
- Use **effective size** when lineup fairness is uncertain or likely compromised.
- Present PPV as a **range** across correction methods, rather than a single curve.

## Relevance for r4lineups
- r4lineups already implements effective size calculations (Tredoux) and CAC/ROC tooling.
- The paper motivates adding functions to compute **PPV ranges** across confidence bins and to visualize the impact of lineup fairness assumptions.

---

If you want a longer version (methods, datasets, or statistical models) or a condensed slide‑style summary, say the word.
