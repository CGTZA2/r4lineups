# Expected Information Gain (EIG) Implementation Summary

## Overview

Successfully implemented the Expected Information Gain (EIG) approach from Starns et al. (2023) for the r4lineups package. EIG provides a comprehensive, information-theoretic measure of the evidentiary value of lineup identification procedures.

## What Was Implemented

### Core Functions

#### 1. **`entropy(p, base = 2)`** - Helper function
- Computes Shannon entropy for a probability value
- Returns 0 for certainty (p = 0 or 1), maximum at p = 0.5
- Uses bits as default unit (base = 2)

#### 2. **`make_eig_data(data, confidence_bins = NULL)`**
- Prepares response category data from trial-level lineup data
- Creates response categories from identification × confidence
- Counts frequencies separately for target-present and target-absent
- Computes likelihoods: p(response|guilty) and p(response|innocent)
- **Input**: Standard r4lineups format (target_present, identification, confidence)
- **Output**: List with response_data, n_guilty, n_innocent, confidence_bins

#### 3. **`compute_eig(eig_data, prior_guilt = 0.5, confidence_bins = NULL)`**
- Core EIG computation following Starns et al. (2023)
- For each response category:
  - Computes posterior probability via Bayes' theorem
  - Calculates information gain: IG = H(prior) - H(posterior)
- Computes Expected Information Gain as weighted average
- **Input**: Either prepared data from make_eig_data() or raw dataframe
- **Output**: lineup_eig object with EIG value and detailed response data

### Visualization Functions

#### 4. **`plot_eig(eig_obj, max_responses = 15, color_by = "identification")`**
- Bar plot showing information gain for each response category
- Categories ordered by IG (descending)
- Color options: by identification decision or by IG magnitude
- Shows EIG value and efficiency percentage in subtitle

#### 5. **`plot_eig_posteriors(eig_obj, max_responses = 15, show_prior = TRUE)`**
- Bar plot showing posterior probabilities of guilt
- Colors distinguish evidence direction (toward guilt vs innocence)
- Dashed line shows prior probability for reference
- Ordered by posterior probability

### Convenience Functions

#### 6. **`make_eig(data, prior_guilt = 0.5, confidence_bins = NULL, show_plot = TRUE, plot_type = "both")`**
- Main wrapper function (similar to make_roc(), make_cac())
- Computes EIG and creates visualizations in one call
- Returns lineup_eig object with embedded plots
- **plot_type** options: "ig", "posteriors", or "both"

### Methods

#### 7. **`print.lineup_eig(x, n_responses = 10, ...)`**
- Custom print method for lineup_eig objects
- Displays EIG, prior, efficiency percentage
- Shows top response categories by information gain
- Summary statistics (sample sizes, confidence bins)

#### 8. **`summary.lineup_eig(object, ...)`**
- Detailed summary of EIG analysis
- Response category statistics (total, average IG, max IG)
- Lists top 3 most informative response categories

## File Structure

```
r4lineups-master/
├── R/
│   └── eig_functions.R          # All EIG functions (378 lines)
├── test_eig.R                   # Test script with examples
├── test_eig_plot.png            # Information gain visualization
├── test_posteriors_plot.png     # Posterior probability visualization
└── EIG_IMPLEMENTATION_SUMMARY.md
```

## Testing Results

Successfully tested with `lineup_example.rda` dataset (N=200):

### Test Results
- **EIG (no binning)**: 0.3691 bits (36.9% efficiency)
- **EIG (with binning)**: 0.2836 bits (28.4% efficiency)
- Binning: c(0, 60, 80, 100) creates low/med/high confidence categories

### Prior Sensitivity
- Prior = 0.3: EIG = 0.2720 bits
- Prior = 0.5: EIG = 0.2836 bits
- Prior = 0.7: EIG = 0.2215 bits

Maximum EIG occurs at prior = 0.5 (maximum uncertainty to resolve)

### Most Informative Responses (with binning)
1. **suspect_(80,100]**: IG = 1.000 bits (perfect information for guilt)
2. **filler_(60,80]**: IG = 0.456 bits (suggests biased lineup)
3. **reject_[0,60]**: IG = 0.166 bits (weak evidence for innocence)

## Integration with r4lineups Package

### Package Patterns Followed
✓ Three-tier function structure (make_*data, compute_*, make_*)
✓ Roxygen2 documentation with @param, @return, @details, @references
✓ Input validation (required columns, valid values)
✓ Custom S3 class (lineup_eig) with print/summary methods
✓ ggplot2 visualizations matching existing style
✓ Compatible with standard data format (target_present, identification, confidence)

### NAMESPACE Updates
The package uses roxygen2 to generate NAMESPACE. Functions are marked with `@export`:
- `make_eig_data()`
- `compute_eig()`
- `plot_eig()`
- `plot_eig_posteriors()`
- `make_eig()`
- `print.lineup_eig()`
- `summary.lineup_eig()`

**Action needed**: Run `devtools::document()` to update NAMESPACE automatically.

## Usage Examples

```r
# Load package and data
library(r4lineups)
data(lineup_example)

# Basic usage
eig_result <- compute_eig(lineup_example)
print(eig_result)

# With confidence binning
eig_binned <- compute_eig(lineup_example,
                          prior_guilt = 0.5,
                          confidence_bins = c(0, 60, 80, 100))

# Full analysis with plots
result <- make_eig(lineup_example,
                   confidence_bins = c(0, 60, 80, 100),
                   show_plot = TRUE,
                   plot_type = "both")

# Access components
result$eig                    # EIG value
result$response_data          # Detailed response data
result$plot_ig               # Information gain plot
result$plot_posteriors       # Posterior probability plot

# Summary
summary(result)
```

## Advantages of EIG

1. **Theoretically grounded**: Based on information theory (Shannon entropy)
2. **Single comprehensive metric**: Unlike partial ROC which requires curve interpretation
3. **Equal-interval scale**: Bits have meaningful interpretation (0 = no info, 1 = perfect info)
4. **Flexible response categories**: Works with any response structure (confidence bins, continuous predictors, etc.)
5. **Comparable across procedures**: Can compare lineups vs showups vs other identification methods
6. **Accounts for full response distribution**: Not just high-confidence IDs

## Comparison with Existing Metrics

| Metric | Type | Unit | Interpretation |
|--------|------|------|----------------|
| **ROC/pAUC** | Discrimination | Probability | Hit rate vs false alarm rate tradeoff |
| **CAC** | Calibration | Proportion | Accuracy at each confidence level |
| **Full ROC** | Discrimination | Probability | Uses ordered decision×confidence categories |
| **EIG** | Information | Bits | Expected information gain about guilt/innocence |

EIG complements existing metrics:
- ROC focuses on discrimination ability
- CAC focuses on calibration/confidence-accuracy relationship
- EIG provides an overall information-theoretic evaluation

## Known Limitations

1. **Prior dependency**: EIG varies with prior probability (though often fixed at 0.5 for lab studies)
2. **Sample size sensitivity**: Requires sufficient data in each response category
3. **Empty cells**: Categories with 0 observations in one condition handled but reduce informativeness
4. **No built-in confidence intervals**: Consider bootstrap methods (can leverage existing r4lineups bootstrap infrastructure)

## Next Steps & Recommendations

### Immediate Actions
1. ✓ Implementation complete
2. ✓ Testing successful
3. ✓ Documentation complete
4. **TODO**: Run `devtools::document()` to update NAMESPACE
5. **TODO**: Run `devtools::check()` to ensure R CMD check passes
6. **TODO**: Add unit tests (use testthat framework if available)

### Future Enhancements
1. **Bootstrap CIs for EIG**: Leverage existing `gen_boot_samples()` infrastructure
2. **Compare two EIGs**: Test if one procedure is more informative than another
3. **PGEIG**: Convert EIG to probability-gain scale for interpretability
4. **Integration with face similarity**: Combine with deepface embeddings to relate similarity to informativeness
5. **Vignette**: Create comparative vignette showing EIG vs ROC vs CAC on same dataset

### 2-HT Model Next?
With EIG successfully implemented, the Two-High Threshold model (Winter et al. 2022) is now ready for implementation. This would require:
- Constrained optimization (via `optim()` with bounds)
- 2×3 contingency table aggregation
- Optional MPTinR integration for G² tests
- More complex but highly valuable for process-level understanding

## References

Starns, J. J., Chen, T., & Staub, A. (2023). Assessing theoretical conclusions via the data they should have produced: A priori comparison of eyewitness identification decision processes using quantitative predictions of the expected information gain. *Psychological Review*.

Shannon, C. E. (1948). A mathematical theory of communication. *Bell System Technical Journal, 27*(3), 379-423.

## Questions or Issues?

Contact the package maintainer or open an issue on the package repository.
