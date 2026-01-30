# ROC Analysis Comparison Guide: Partial vs. Full

## Overview

The r4lineups package now includes TWO methods for ROC analysis of eyewitness lineup data:

1. **Partial ROC** (`make_roc`) - Traditional method (Wixted & Mickes, 2012)
2. **Full ROC** (`make_fullroc`) - New method (Smith & Yang, 2020)

This guide helps you choose which method to use.

## Quick Decision Tree

```
Are you replicating a specific published study that used partial ROC?
├─ YES → Use make_roc() for comparability
└─ NO → Use make_fullroc() (recommended for new analyses)
```

## Side-by-Side Comparison

| Feature | Partial ROC<br>`make_roc()` | Full ROC<br>`make_fullroc()` |
|---------|---------------------------|----------------------------|
| **Method** | Wixted & Mickes (2012) | Smith & Yang (2020) |
| **Status** | Traditional/Legacy | Modern/Recommended |
| | | |
| **WHAT IT USES** | | |
| Suspect IDs | ✓ Yes | ✓ Yes |
| Filler IDs | ✗ No | ✓ Yes |
| Rejections | ✗ No | ✓ Yes |
| | | |
| **MEASUREMENTS** | | |
| Measure | pAUC (partial area) | AUC (full area) |
| Range | Limited (0 to max suspect ID) | Full (0 to 1.0) |
| Typical max X-axis | 0.10 - 0.40 | 1.0 |
| | | |
| **INTERPRETATION** | | |
| Discriminability | Confounded with bias | Pure discriminability |
| Threshold-free | ✗ No | ✓ Yes |
| Comparable across studies | Limited | ✓ Yes |
| | | |
| **WHAT IT MEASURES** | | |
| Conceptually measures | Witness memory (sort of) | Investigator discriminability |
| Evidence type | Inculpatory only | Inculpatory + exculpatory |
| Decision maker focus | Eyewitness | Police investigator |
| | | |
| **PRACTICAL ISSUES** | | |
| Bias toward conservative | ✓ Yes (problematic) | ✗ No |
| Handling filler IDs | Divides by lineup size | Uses directly |
| Handling rejections | Ignores | Uses directly |
| | | |
| **WHEN TO USE** | | |
| New research | Not recommended | ✓ Recommended |
| Replicating old studies | ✓ Yes | Maybe not |
| Policy decisions | Not ideal | ✓ Ideal |
| Theory testing | Limited | ✓ Better |

## Detailed Comparison

### 1. What They Measure

#### Partial ROC (`make_roc`)
- **Concept**: How well can we distinguish guilty from innocent suspects using ONLY suspect identifications?
- **Focus**: Eyewitness memory performance (partially)
- **Evidence**: Only inculpatory (suspect IDs)
- **Problem**: Ignores most of the data!

#### Full ROC (`make_fullroc`)
- **Concept**: How well can an investigator distinguish guilty from innocent suspects using ALL witness evidence?
- **Focus**: Investigator discriminability
- **Evidence**: Both inculpatory (suspect IDs) AND exculpatory (filler IDs, rejections)
- **Advantage**: Uses complete information

### 2. How They Work

#### Partial ROC Algorithm

```r
For each confidence level (high to low):
  HR = P(suspect ID | target present)
  FAR = P(suspect ID | target absent) + P(filler ID | target absent) / lineup_size
  Plot (FAR, HR)
Compute pAUC over limited range
```

**Key point**: Filler IDs are converted to estimated suspect IDs by dividing by lineup size. Rejections are completely ignored.

#### Full ROC Algorithm

```r
For each decision × confidence category:
  HR = P(response | target present)
  FAR = P(response | target absent)
  DR = HR / FAR
Sort categories by DR (evidence strength)
Compute cumulative HR and FAR
Plot full ROC curve
Compute full AUC over entire [0,1] range
```

**Key point**: ALL responses are used directly. Ordering is by diagnosticity ratio (evidence strength).

### 3. The "Partial Area" Problem

#### Why pAUC is Biased

Consider two procedures:
- **Conservative**: Low suspect ID rate (20%), low false ID rate (5%)
- **Liberal**: High suspect ID rate (40%), high false ID rate (15%)

**Partial ROC**:
- Conservative curve: steep slope over narrow range [0, 0.05]
- Liberal curve: moderate slope over wider range [0, 0.15]
- Comparing pAUC over [0, 0.05]: Conservative wins
- **But**: This ignores 75% of the liberal procedure's suspect IDs!

**Full ROC**:
- Conservative curve: steep then flat
- Liberal curve: moderate throughout
- Comparing full AUC [0, 1.0]: Fair comparison
- **Both procedures can realize their full potential**

#### Mathematical Demonstration

```
Conservative Procedure:
  Suspect IDs: 20% (TP), 5% (TA)  → pAUC region
  Filler IDs:  30% (TP), 40% (TA) → ignored in partial ROC
  Rejections:  50% (TP), 55% (TA) → ignored in partial ROC

Liberal Procedure:
  Suspect IDs: 40% (TP), 15% (TA) → only 5% used in pAUC comparison!
  Filler IDs:  25% (TP), 35% (TA) → ignored
  Rejections:  35% (TP), 50% (TA) → ignored
```

**pAUC** compares all of conservative's suspect IDs to only 1/3 of liberal's suspect IDs. **Unfair!**

**Full AUC** uses everything. **Fair!**

### 4. Threshold-Free Discriminability

#### Partial ROC: NOT Threshold-Free

- Covers limited range (e.g., 0 to 0.30)
- Comparison depends on which range you choose
- Different procedures span different ranges
- Cannot achieve true threshold-free measure

#### Full ROC: Truly Threshold-Free

- Covers full range [0, 1.0]
- All procedures span same range
- Independent of any specific false alarm rate
- True measure of discriminability

### 5. Example Comparison

```r
library(r4lineups)

# Same data, both methods
data <- your_lineup_data

# Partial ROC
partial <- make_roc(data)
cat("pAUC:", partial$pauc, "\n")
cat("Range:", range(partial$roc_data$false_id_rate), "\n")

# Full ROC
full <- make_fullroc(data)
cat("Full AUC:", full$auc, "\n")
cat("Range:", range(full$roc_data$cumulative_false_alarm_rate), "\n")

# THESE VALUES ARE NOT COMPARABLE!
```

**Output might look like**:
```
Partial ROC:
  pAUC: 0.045
  Range: 0 0.18

Full ROC:
  Full AUC: 0.72
  Range: 0 1
```

**DO NOT** compare 0.045 to 0.72! They measure different things over different ranges.

## When to Use Each Method

### Use Partial ROC (`make_roc`) When:

1. **Replicating a specific study**
   - The original used partial ROC
   - You need direct comparability
   - Example: "We replicated Experiment 1 from Smith et al. (2018)"

2. **Meta-analysis of old literature**
   - Combining results from partial ROC studies
   - Maintaining consistency with included studies

3. **Journal requires it**
   - Some reviewers may be unfamiliar with full ROC
   - Consider including both methods

4. **Comparing to established benchmarks**
   - Field has established pAUC norms
   - Your goal is to compare to those norms

### Use Full ROC (`make_fullroc`) When:

1. **New research** (RECOMMENDED)
   - Fresh experiments
   - Novel procedures
   - Policy recommendations

2. **Unbiased comparisons needed**
   - Comparing procedures with different conservativism
   - Fair head-to-head comparisons
   - Procedure validation

3. **Complete evidence matters**
   - Filler IDs and rejections are important
   - Theoretical work on diagnosticity
   - Understanding exculpatory evidence

4. **Following NAS 2014 recommendations**
   - National Academy of Sciences called for using all evidence
   - Full ROC addresses this

5. **Publishing in modern journals**
   - Method is gaining acceptance
   - Theoretically superior
   - Future of the field

## Transitional Strategy

If you want to be thorough:

```r
# Run both analyses
partial <- make_roc(data)
full <- make_fullroc(data)

# Report both
cat("Traditional partial ROC: pAUC =", partial$pauc, "\n")
cat("Full ROC (Smith & Yang): AUC =", full$auc, "\n")

# Primary conclusions from full ROC
# Partial ROC for comparison with literature
```

**In manuscript**:
> "Following Smith et al. (2020), we conducted full ROC analysis (AUC = 0.78). For comparison with prior literature, we also report partial ROC (pAUC = 0.052, comparable to Jones & Smith, 2018)."

## Common Misconceptions

### ❌ WRONG: "pAUC and full AUC measure the same thing on different scales"

✓ **CORRECT**: They measure different things:
- pAUC: Performance over limited operating range (suspect IDs only)
- Full AUC: Discriminability using all evidence (suspect IDs + filler IDs + rejections)

### ❌ WRONG: "Higher AUC means higher pAUC"

✓ **CORRECT**: Not necessarily! A conservative procedure can have:
- High pAUC (steep slope over narrow range)
- Moderate full AUC (after including all evidence)

### ❌ WRONG: "Partial ROC is simpler and easier to interpret"

✓ **CORRECT**: Actually reversed:
- Partial ROC interpretation is complicated (confounded with bias, limited range)
- Full ROC interpretation is straightforward (pure discriminability, 0.5 = chance, 1.0 = perfect)

### ❌ WRONG: "Full ROC is just a fancier version of partial ROC"

✓ **CORRECT**: Conceptually different:
- Partial ROC: eyewitness memory (sort of)
- Full ROC: investigator discriminability

Different theoretical foundations!

## Recommendations by Use Case

### For Researchers

| Research Goal | Recommended Method | Rationale |
|---------------|-------------------|-----------|
| **Testing new procedure** | Full ROC | Unbiased, complete |
| **Replicating old study** | Partial ROC | Comparability |
| **Memory manipulation** | Full ROC | Better discriminability measure |
| **System variable** | Full ROC | Policy implications |
| **Comparing to literature** | Both | Full ROC primary, partial for context |

### For Policymakers

**Always use Full ROC**

Reasons:
1. Unbiased comparisons between procedures
2. Uses all available evidence
3. Aligns with NAS 2014 recommendations
4. Threshold-free (doesn't depend on specific operating point)
5. Reflects real investigator decision-making

### For Students

**Learn both, prefer Full ROC**

1. **Understand partial ROC**:
   - Read Wixted & Mickes (2012)
   - Understand historical context
   - Know its limitations

2. **Master full ROC**:
   - Read Smith et al. (2020)
   - Understand the theory
   - Use for your research

3. **Be bilingual**:
   - Can discuss both methods
   - Know when each is appropriate
   - Can explain differences to others

## Technical Note: Why pAUC is Biased

### The Geometry

Holding discriminability constant:
- Conservative procedure: steep slope initially, then flat
- Liberal procedure: moderate slope throughout

When comparing over conservative procedure's range:
- Conservative: full slope captured
- Liberal: only initial (steep) part captured

Result: **pAUC systematically favors conservative procedures**

This is not a software issue—it's inherent to partial ROC!

### The Solution

Full ROC allows both procedures to realize their full range:
- Conservative: steep then flat (balances out)
- Liberal: moderate throughout (full contribution)

Result: **Full AUC provides fair comparison**

## Summary Table

|  | Partial ROC | Full ROC |
|--|-------------|----------|
| **For new research** | ✗ | ✓ |
| **Theoretically sound** | Limited | ✓ |
| **Unbiased** | ✗ | ✓ |
| **Complete evidence** | ✗ | ✓ |
| **Threshold-free** | ✗ | ✓ |
| **Historical precedent** | ✓ | Growing |
| **Replicating old work** | ✓ | ✗ |
| **Ease of interpretation** | ✗ | ✓ |
| **NAS 2014 compliant** | ✗ | ✓ |

## Bottom Line

### For Most Users

**Use `make_fullroc()` for new analyses.**

It's:
- Theoretically superior
- Unbiased
- Complete
- Easier to interpret
- The future of the field

**Use `make_roc()` only when**:
- Replicating specific studies
- Comparing to old literature
- Required by reviewers (consider offering both)

## References

**Full ROC**:
Smith, A. M., Yang, Y., & Wells, G. L. (2020). Distinguishing between investigator discriminability and eyewitness discriminability. *Perspectives on Psychological Science, 15*(3), 589-607.

**Partial ROC**:
Wixted, J. T., & Mickes, L. (2012). The field of eyewitness memory should abandon probative value and embrace receiver operating characteristic analysis. *Perspectives on Psychological Science, 7*(3), 275-278.

**Critique of pAUC**:
Lampinen, J. M., Smith, A. M., & Wells, G. L. (2019). Four utilities in eyewitness identification practice. *Law and Human Behavior, 43*(1), 26-44.

**NAS Report**:
National Research Council. (2014). *Identifying the culprit: Assessing eyewitness identification*. Washington, DC: The National Academies Press.
