# Face Similarity Analysis

> **Acknowledgement.** Large language models supplied by Anthropic and
> OpenAI were used in preparing this vignette, to assist with checking
> code, testing the veracity of the approaches described, and on
> occasion editing the text. Responsibility for the content rests with
> the authors.

> **Note on the code in this vignette.** Unlike the other vignettes in
> this package, the examples below are **not executed** when it is
> built. They require two things the package cannot supply: a working
> Python environment with DeepFace installed, and **face image files of
> your own**. No face images are distributed with r4lineups, for obvious
> reasons. The code is exactly what would run against your own images,
> but the outputs shown in the prose are described rather than computed.

## Introduction

**r4lineups** provides deep learning methods for measuring facial
similarity in eyewitness lineups. These tools complement mock-witness
studies by giving analysts reproducible, continuous similarity summaries
that can help screen candidate foils and document visual fairness
checks. They should be treated as exploratory aids, not as a replacement
for mock-witness or outcome-based evidence.

### Why measure face similarity?

Traditional lineup fairness assessment relies on mock witnesses viewing
the lineup and choosing from it **on the basis of a description**. That
dependence is the method’s foundation, and it is also where its
difficulties lie.

**Whose description?** The mock-witness procedure asks people to pick
the member who best matches a description, so the result is a property
of the description-lineup pair, not of the lineup alone. Change the
description and the measured fairness changes. In real cases the
description is the witness’s own, given under conditions that may have
been poor, and it is often the only one available. In laboratory studies
it is frequently fabricated by the experimenters, which raises a
different problem: a description written with a great deal of detail
makes a lineup very hard to construct, because few candidate foils will
match on every feature. Detail also tends to narrow the field towards
the suspect, so an over-specified description can make a perfectly
reasonable lineup look unfair — and an under-specified one can make an
unfair lineup look acceptable.

Beyond that, the procedure requires recruiting and testing participants,
which is slow and costly, and returns categorical data — which member
each mock witness chose — rather than a continuous measure of
resemblance.

**Deep learning face similarity** measures something different: the
resemblance between the images themselves, with no description and no
participants. It is numeric, fast, scalable to hundreds of lineups, and
exactly reproducible — the same images always give the same scores. It
can also say *which* foils sit close to or far from the suspect, which a
choice distribution can only do indirectly.

### An important caution before going further

It is tempting to move from “we can measure similarity” to “here is the
right amount of similarity”, and this vignette deliberately does not do
that.

**The literature is not settled on how similar lineup foils should be.**
The long-standing intuition that foils should closely resemble the
suspect competes with evidence that very high similarity can impair
discriminability by removing the diagnostic features a witness might
use, and the balance appears to depend on the procedure, the encoding
conditions and what is being optimised. There is no agreed optimum, and
no threshold at which a lineup becomes “fair” on a similarity metric.

So the functions here **report degree of similarity**; they do not grade
it. Where the sections below give numbers, read them as descriptions of
the images, not as verdicts on the lineup. Similarity scores are most
defensible used comparatively — this foil against that one, this lineup
against another constructed the same way — and as a screening aid during
construction, rather than as evidence of fairness after the fact. The
choice-based measures in the main vignette, and the outcome-based
measures elsewhere in the package, remain the evidence about how a
lineup actually performed.

### What This Vignette Covers

1.  **Setup** - Installing Python dependencies
2.  **Basic Comparison** - Comparing two face images
3.  **Lineup Analysis** - Analyzing entire lineups
4.  **Models & Metrics** - Choosing the right configuration
5.  **Visualization** - Displaying results
6.  **Advanced Features** - Embeddings, batch processing, dimensionality
    reduction
7.  **Integration** - Combining with traditional measures
8.  **Interpretation** - What these scores do and do not establish

## Setup

Face similarity analysis uses Python’s DeepFace library via R’s
reticulate package. This requires a one-time setup.

### Check Current Setup

``` r

library(r4lineups)

# Check if Python and dependencies are available
check_python_deps()
```

### Install Python Dependencies

If you don’t have the required dependencies, install them:

The setup has one requirement that catches most people out, so it is
worth doing in the right order.

**TensorFlow, which DeepFace depends on, does not publish wheels for
every Python version.** At the time of writing it supports Python 3.9 to
3.12. If your system Python is newer — 3.13 or 3.14, which is
increasingly common — the install will proceed a long way and then fail
with `No matching distribution found for tensorflow`.
[`install_r4lineups_python()`](https://cgtza2.github.io/r4lineups/reference/install_r4lineups_python.md)
checks the interpreter first and stops before downloading anything,
rather than letting that happen.

So pin the version, and **obtain a suitable interpreter before asking
for an environment built from one**:

``` r

# Step 1: get a Python that TensorFlow supports. Skip if you already have one.
reticulate::install_python(version = "3.11")

# Step 2: build the environment from it and install DeepFace
install_r4lineups_python(envname = "r4lineups-face-env",
                         python_version = "3.11")
```

Requesting `python_version = "3.11"` without step 1 fails, because there
is no 3.11 interpreter to build from. Earlier versions of this package
passed that straight through to reticulate, which reported

    Suitable Python installation for creating a venv not found.
    Requested version constraint: 3.11

without saying how to obtain one. The package now checks first and
reports which versions it did find, along with the
[`reticulate::install_python()`](https://rstudio.github.io/reticulate/reference/install_python.html)
call needed.

The first run takes several minutes, as it downloads the deep learning
models (ArcFace, FaceNet and others). They are cached afterwards.

### If the Python setup defeats you

It is a genuine possibility — the dependency chain is long and platform
dependent. Two fallbacks:

- The bundled Shiny application,
  [`run_r4lineups_app()`](https://cgtza2.github.io/r4lineups/reference/run_r4lineups_app.md),
  covers the lineup fairness measures that need no Python at all.
- Everything in the rest of this package — effective size, functional
  size, ROC, CAC, the SDT and MPT models — is pure R. Face similarity is
  the only component with a Python dependency, and it is an optional
  adjunct rather than a prerequisite for anything else.

### Verify Installation

``` r

# Check available models
available_models()

# Check available face detectors
available_detectors()
```

## Basic Face Comparison

The simplest use case is comparing two face images.

### Compare Two Faces

``` r

# Compare target to a foil
# (Use your own image paths)
result <- face_similarity(
  img1_path = "path/to/target.jpg",
  img2_path = "path/to/foil1.jpg",
  model = "ArcFace",     # State-of-the-art face recognition model
  metric = "cosine"      # Similarity metric
)

print(result)
```

### Understanding the Output

``` r

# Similarity score (0-1, higher = more similar)
result$similarity  # e.g., 0.85 = very similar

# Distance (lower = more similar)
result$distance    # e.g., 0.15

# Would the model judge these to be the same person?
result$verified    # see the note below on what this does and does not mean

# Model's threshold for verification
result$threshold   # e.g., 0.68 for ArcFace + cosine
```

**Reading the output.** `similarity` is a continuous score on the
model’s own scale; higher means the two images are closer in the model’s
embedding space. There is deliberately no table of bands here, because —
as set out above — the literature does not establish what level of
similarity a lineup foil ought to have. Compare scores against each
other, not against a standard.

`verified` is different, and it is worth separating clearly. It is not a
judgement about fairness; it is the model’s **identity** decision —
whether the two images are of the *same person*. A `TRUE` here does
carry a definite implication, but a narrow one: if the model cannot
distinguish the foil from the suspect, the foil is arguably not
functioning as an alternative at all, and the lineup may effectively
contain the suspect twice. That is a construction error, detectable
without any claim about optimal similarity.

`threshold` is the model’s own cut-off for that identity decision, and
it varies by model and metric — it is not a fairness criterion.

## Lineup Similarity Analysis

Analyze all lineup members against the target in one call.

### Analyze Complete Lineup

``` r

# Analyze all foils in a lineup
lineup_result <- lineup_similarity(
  target_path = "path/to/target.jpg",
  foil_paths = c(
    "path/to/foil1.jpg",
    "path/to/foil2.jpg",
    "path/to/foil3.jpg",
    "path/to/foil4.jpg",
    "path/to/foil5.jpg"
  ),
  model = "ArcFace",
  metric = "cosine"
)

print(lineup_result)
```

### Examining Results

``` r

# View as dataframe
lineup_result

# Summary statistics
summary(lineup_result)

# Foils the model judges to be the same identity as the suspect. These are
# worth inspecting by eye: a foil indistinguishable from the suspect is not
# functioning as an alternative.
same_identity <- lineup_result[lineup_result$verified, ]
if (nrow(same_identity) > 0) {
  cat("Model judges these foils to be the same person as the target:\n")
  print(same_identity[, c("foil_path", "similarity")])
}

# Check similarity distribution
cat("Mean similarity:", round(mean(lineup_result$similarity), 3), "\n")
cat("SD similarity:", round(sd(lineup_result$similarity), 3), "\n")
cat("Range:", range(lineup_result$similarity), "\n")
```

### Describing the similarity profile

The heading here is deliberately not “fairness assessment”. What these
numbers describe is the **similarity profile** of the lineup — where the
foils sit relative to the suspect and to each other. Turning that into a
verdict would require knowing what profile is desirable, which is
exactly what is unsettled.

Three quantities are worth reporting:

- **Mean similarity** — how close the foils are to the suspect on
  average.
- **Spread of similarity** — whether the foils are homogeneous, or
  whether one or two sit much closer to the suspect than the rest. This
  is often the more informative of the two, since a lineup where one
  foil resembles the suspect far more than the others is unbalanced
  whatever the mean.
- **Identity matches** — foils the model cannot distinguish from the
  suspect, as discussed above.

``` r

# Describe the similarity profile
mean_sim <- mean(lineup_result$similarity)
sd_sim <- sd(lineup_result$similarity)
n_verified <- sum(lineup_result$verified)

cat("Similarity profile:\n")
cat("  Mean similarity:", round(mean_sim, 3), "\n")
cat("  SD of similarity:", round(sd_sim, 3), "\n")
cat("  Range:", round(range(lineup_result$similarity), 3), "\n")
cat("  Foils judged same identity:", n_verified, "\n\n")

# The one flag worth raising automatically, because it is an identity decision
# rather than a fairness judgement
if (n_verified > 0) {
  cat("Note:", n_verified, "foil(s) judged the same person as the target.\n")
  cat("Inspect these by eye; they may not be functioning as alternatives.\n")
}
```

## Models and Metrics

Different models and metrics serve different purposes.

### Available Models

``` r

# List all models
available_models()

# Models in order of recommendation for lineup research:
# 1. ArcFace    - State-of-the-art, best overall performance
# 2. Facenet512 - Very good, slightly faster than ArcFace
# 3. Facenet    - Good, faster inference
# 4. VGG-Face   - Older but reliable baseline
# 5. OpenFace   - Fastest, lower accuracy
```

### Distance Metrics

``` r

# Three metrics available:

# 1. Cosine (RECOMMENDED for most cases)
#    - Range: [0, 1] (can convert to similarity)
#    - Focuses on angle between embeddings
#    - Most commonly used in face recognition

# 2. Euclidean L2 (normalized Euclidean)
#    - Normalized vector distance
#    - Good for comparing across models

# 3. Euclidean
#    - Raw vector distance
#    - Less commonly used
```

### Comparing Models

``` r

# Compare different models on same lineup
models_to_test <- c("ArcFace", "Facenet512", "VGG-Face")

comparison_results <- list()

for (model in models_to_test) {
  result <- lineup_similarity(
    target_path = "path/to/target.jpg",
    foil_paths = foil_paths,
    model = model,
    metric = "cosine"
  )

  comparison_results[[model]] <- data.frame(
    model = model,
    mean_similarity = mean(result$similarity),
    sd_similarity = sd(result$similarity),
    n_verified = sum(result$verified)
  )
}

comparison_df <- do.call(rbind, comparison_results)
print(comparison_df)
```

**Interpretation:** - Models may give different absolute scores - But
relative ordering (which foils are most similar) usually consistent -
Use same model consistently within a study - ArcFace generally most
accurate

## Visualization

### Display Lineup with Similarity Scores

``` r

# Create visual display of lineup
display_lineup(
  target_path = "path/to/target.jpg",
  foil_paths = foil_paths,
  similarity_scores = lineup_result$similarity,
  labels = paste("Foil", 1:5),
  title = "Lineup Similarity Analysis"
)

# Save the visualization
ggsave("lineup_display.png", width = 10, height = 8)
```

### Similarity Heatmap

``` r

# For multiple lineups, create heatmap
library(ggplot2)

# Suppose we have analyzed multiple lineups
lineup_ids <- c("Lineup_1", "Lineup_2", "Lineup_3")
foil_positions <- 1:5

# Create example data (replace with your actual data)
similarity_matrix <- matrix(
  runif(15, 0.3, 0.8),  # Example data
  nrow = 3,
  ncol = 5,
  dimnames = list(lineup_ids, paste("Foil", foil_positions))
)

# Convert to long format for ggplot
library(reshape2)
sim_long <- melt(similarity_matrix)
colnames(sim_long) <- c("Lineup", "Foil", "Similarity")

# Create heatmap
ggplot(sim_long, aes(x = Foil, y = Lineup, fill = Similarity)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", mid = "yellow", high = "red",
    midpoint = 0.6,
    limits = c(0, 1)
  ) +
  geom_text(aes(label = round(Similarity, 2)), color = "black", size = 3) +
  theme_minimal() +
  labs(title = "Lineup Similarity Heatmap",
       subtitle = "Color indicates target-foil similarity")
```

### Similarity Distribution

``` r

# Plot distribution of similarities
ggplot(lineup_result, aes(x = similarity)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black") +
  geom_vline(aes(xintercept = threshold),
             color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = threshold, y = Inf,
           label = "Verification\nThreshold",
           vjust = 1.5, color = "red") +
  labs(
    title = "Distribution of Target-Foil Similarity",
    x = "Similarity Score",
    y = "Count"
  ) +
  theme_minimal()
```

## Advanced Features

### Extract Face Embeddings

Embeddings are the deep learning representations of faces. They can be
used for advanced analyses.

``` r

# Get embedding for a face
embedding <- get_embedding(
  img_path = "path/to/face.jpg",
  model = "ArcFace"
)

# Embedding is a numeric vector (typically 512 dimensions)
length(embedding)  # 512 for ArcFace

# Get embeddings for comparison
result_with_emb <- face_similarity(
  img1_path = "path/to/target.jpg",
  img2_path = "path/to/foil.jpg",
  model = "ArcFace",
  return_embeddings = TRUE
)

target_embedding <- result_with_emb$embeddings$img1
foil_embedding <- result_with_emb$embeddings$img2
```

### Dimensionality Reduction Visualization

Visualize lineup in 2D space using embeddings:

``` r

# Requires Rtsne package
library(Rtsne)

# Collect embeddings for all lineup members
all_embeddings <- list()

# Target embedding
all_embeddings[["Target"]] <- get_embedding("path/to/target.jpg", model = "ArcFace")

# Foil embeddings
for (i in seq_along(foil_paths)) {
  all_embeddings[[paste0("Foil_", i)]] <- get_embedding(foil_paths[i], model = "ArcFace")
}

# Convert to matrix
embedding_matrix <- do.call(rbind, all_embeddings)

# Run t-SNE
set.seed(42)
tsne_result <- Rtsne(embedding_matrix, dims = 2, perplexity = 2)

# Create visualization dataframe
viz_df <- data.frame(
  x = tsne_result$Y[, 1],
  y = tsne_result$Y[, 2],
  member = names(all_embeddings),
  type = ifelse(names(all_embeddings) == "Target", "Target", "Foil")
)

# Plot
ggplot(viz_df, aes(x = x, y = y, color = type, label = member)) +
  geom_point(size = 4) +
  geom_text(vjust = -1) +
  scale_color_manual(values = c("Target" = "red", "Foil" = "blue")) +
  labs(
    title = "Lineup Members in Embedding Space",
    subtitle = "t-SNE projection of face embeddings",
    x = "t-SNE Dimension 1",
    y = "t-SNE Dimension 2"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

### Batch Processing Multiple Lineups

``` r

# Process multiple lineups efficiently
lineup_list <- list(
  lineup1 = list(target = "target1.jpg", foils = c("1a.jpg", "1b.jpg", "1c.jpg")),
  lineup2 = list(target = "target2.jpg", foils = c("2a.jpg", "2b.jpg", "2c.jpg")),
  lineup3 = list(target = "target3.jpg", foils = c("3a.jpg", "3b.jpg", "3c.jpg"))
)

# Analyze all lineups
results_list <- lapply(names(lineup_list), function(lineup_name) {
  lineup <- lineup_list[[lineup_name]]

  result <- lineup_similarity(
    target_path = lineup$target,
    foil_paths = lineup$foils,
    model = "ArcFace"
  )

  # Add lineup identifier
  result$lineup_id <- lineup_name

  result
})

# Combine results
all_results <- do.call(rbind, results_list)

# Summary by lineup
library(dplyr)
lineup_summary <- all_results %>%
  group_by(lineup_id) %>%
  summarise(
    mean_similarity = mean(similarity),
    sd_similarity = sd(similarity),
    min_similarity = min(similarity),
    max_similarity = max(similarity),
    n_verified = sum(verified)
  )

print(lineup_summary)
```

## Integration with Traditional Measures

Combine deep learning similarity with traditional lineup fairness
measures.

### Compare with Effective Size

``` r

# Traditional effective size from mock witnesses
# (Using example data)
mock_choices <- c(3, 2, 5, 6, 1, 3, 3, 5, 6, 4)  # Position 3 is suspect
trad_eff_size <- esize_T(table(factor(mock_choices, levels = 1:6)))

cat("Traditional effective size:", round(trad_eff_size, 2), "\n")

# Face similarity, for comparison. Note these measure different things:
# effective size describes how mock witnesses distributed their choices,
# similarity describes the images themselves.
mean_similarity <- mean(lineup_result$similarity)

cat("Mean face similarity:", round(mean_similarity, 3), "\n")

# Correlation across multiple lineups
# (With sufficient data, examine relationship)
```

### Similarity-Weighted Effective Size

Create a hybrid measure:

The direction of the relationship matters here, and it is easy to state
backwards. Mock witnesses choose the member who best matches **the
description**. A foil that closely resembles the suspect also resembles
the description, so it draws choices *away* from the suspect — which is
precisely what makes a lineup fair. A foil that resembles the suspect
very little matches the description poorly and tends to be chosen by
nobody: it is a null foil, and it contributes nothing.

So the expectation runs: **higher foil-suspect similarity should mean a
higher share of mock-witness choices**, and hence a larger effective
size.

``` r

# Expected selection shares implied by similarity. Foils more similar to the
# suspect match the description better and should attract more choices.
similarities <- lineup_result$similarity
expected_prob <- similarities / sum(similarities)

# Compare to observed selections
# observed_prob <- table(mock_choices[-which(mock_choices == suspect_pos)]) /
#                  length(mock_choices[-which(mock_choices == suspect_pos)])

# Similarity-adjusted bias metric
# (Larger discrepancy = more bias after accounting for appearance)
```

### Validation Studies

Examine predictive validity:

``` r

# Research question: Do objective similarity scores predict mock witness choices?

# For each lineup:
# - Get face similarity scores
# - Collect mock witness data
# - Correlate: Do more similar foils get chosen more often?

# Example correlation structure (use your data):
# cor.test(foil_similarities, foil_selection_rates)
```

## Practical Guidelines

### Interpreting Similarity Scores

**Similarity ranges (for ArcFace + cosine):** - `> 0.80`: Very high
similarity - likely lineup bias - `0.70-0.80`: High similarity - check
if acceptable for your context - `0.55-0.70`: Moderate similarity -
generally acceptable - `0.40-0.55`: Low similarity - foils sufficiently
different - `< 0.40`: Very low similarity - foils may not match
description

**Context matters:** - Acceptable similarity depends on description
specificity - More specific descriptions allow tighter similarity
ranges - Less specific descriptions require broader ranges

### Best Practices

1.  **Consistency**
    - Use same model throughout a study
    - Use same metric throughout a study
    - Use same verification thresholds
2.  **Validation**
    - Compare similarity scores to mock witness data when available
    - Validate that your similarity threshold makes sense for your
      context
3.  **Complementarity**
    - Use face similarity alongside traditional mock witness studies
    - Objective similarity + subjective judgments provide comprehensive
      assessment
4.  **Documentation**
    - Report model and metric used
    - Report mean and SD of similarities
    - Report verification threshold and number verified
5.  **Image Quality**
    - Use high-quality, front-facing photos
    - Ensure good lighting and resolution
    - Consistent photo conditions across lineup members

### Troubleshooting

**“No face detected”** - Image quality too poor - Face not front-facing
enough - Try different detector: `detector = "opencv"` is more lenient

**Embeddings fail** - Check image file paths are correct - Ensure images
are standard formats (jpg, png) - Verify Python environment is activated

**Inconsistent results** - Deep learning models are deterministic - If
results change, check if images were modified - Verify same model/metric
used

**Very high/low similarities** - Check image quality - Verify correct
target-foil pairing - Consider if lineup truly is biased/fair

## Example Workflow

Complete analysis from start to finish:

``` r

# 1. Setup (first time only)
# install_r4lineups_python()

# 2. Load package
library(r4lineups)

# 3. Analyze lineup
lineup_sim <- lineup_similarity(
  target_path = "suspect_photo.jpg",
  foil_paths = c("foil1.jpg", "foil2.jpg", "foil3.jpg",
                 "foil4.jpg", "foil5.jpg"),
  model = "ArcFace",
  metric = "cosine"
)

# 4. Assess fairness
mean_sim <- mean(lineup_sim$similarity)
sd_sim <- sd(lineup_sim$similarity)
n_verified <- sum(lineup_sim$verified)

# 5. Generate report
cat("=== Lineup Fairness Report ===\n\n")
cat("Objective Similarity Analysis\n")
cat("Model: ArcFace | Metric: Cosine\n\n")
cat("Results:\n")
cat("  Mean similarity:", round(mean_sim, 3), "\n")
cat("  SD similarity:", round(sd_sim, 3), "\n")
cat("  Range:", round(min(lineup_sim$similarity), 3), "-",
    round(max(lineup_sim$similarity), 3), "\n")
cat("  Verified as same person:", n_verified, "of 5 foils\n\n")

if (mean_sim > 0.7 || n_verified > 0) {
  cat("CONCERN: High similarity indicates potential lineup bias\n")
} else if (mean_sim < 0.55) {
  cat("ACCEPTABLE: Foils sufficiently dissimilar from target\n")
} else {
  cat("BORDERLINE: Consider additional assessment\n")
}

# 6. Visualize
display_lineup(
  target_path = "suspect_photo.jpg",
  foil_paths = c("foil1.jpg", "foil2.jpg", "foil3.jpg",
                 "foil4.jpg", "foil5.jpg"),
  similarity_scores = lineup_sim$similarity,
  title = "Lineup 001 - Similarity Analysis"
)

# 7. Save results
write.csv(lineup_sim, "lineup_001_similarity.csv", row.names = FALSE)
```

## References

**Deep Learning Face Recognition:** - Deng, J., Guo, J., Xue, N., &
Zafeiriou, S. (2019). ArcFace: Additive angular margin loss for deep
face recognition. *CVPR 2019*.

- Schroff, F., Kalenichenko, D., & Philbin, J. (2015). FaceNet: A
  unified embedding for face recognition and clustering. *CVPR 2015*.

**DeepFace Library:** - Serengil, S. I., & Ozpinar, A. (2020).
LightFace: A hybrid deep face recognition framework. *ASYU 2020*.

**Lineup Research Applications:** - Tredoux, C. G. (2002). A direct
measure of facial similarity and its relation to human similarity
perceptions. *Journal of Experimental Psychology: Applied, 8*(3),
180-193.

- Colloff, M. F., Wade, K. A., & Strange, D. (2016). Unfair lineups make
  witnesses more likely to confuse innocent and guilty suspects.
  *Psychological Science, 27*(9), 1227-1239.

## Technical Details

### What is computed

Each image is passed through a pretrained deep face-recognition network,
which maps it to an **embedding** — a vector, typically 128 or 512
dimensional, positioned so that images of the same person land close
together. Similarity between two faces is then a distance in that space,
converted to a similarity score.

| Component | Meaning |
|----|----|
| `model` | Which network produces the embedding (ArcFace, FaceNet, VGG-Face, …). Different networks give different embedding spaces, so scores are **not comparable across models** |
| `metric` | How distance is measured in that space — cosine, Euclidean, or Euclidean on L2-normalised vectors |
| `similarity` | Distance converted to a 0-1 scale, higher meaning closer |
| `threshold` | The model’s own cut-off for judging two images the same **identity**. Model- and metric-specific |
| `verified` | Whether the distance falls inside that threshold |

### Assumptions and limitations

1.  **Scores are model-relative.** ArcFace and FaceNet place faces in
    different spaces. Never compare a similarity score from one against
    another, and always report which model and metric produced a number.
2.  **The networks were trained for recognition, not for lineup
    construction.** They are optimised to tell identities apart, which
    is related to but not the same as capturing what makes two faces
    look alike to a witness. Whether the embedding distance corresponds
    to *human* perceived similarity is an empirical question, and the
    appropriate reference here is Tredoux (2002) on direct similarity
    measurement and its relation to human judgements.
3.  **Image conditions matter.** Pose, lighting, expression, resolution
    and background all affect embeddings. Lineup photographs taken under
    different conditions may differ in similarity for reasons that have
    nothing to do with facial resemblance. Standardised images are
    essential.
4.  **Demographic differences in accuracy** are well documented for face
    recognition systems, with error rates varying by ethnicity, sex and
    age. That is a serious caution in a forensic context, where the
    consequences of systematic error fall unevenly.
5.  **No fairness threshold exists.** The point of the caution at the
    top of this vignette, restated: these functions describe similarity,
    they do not adjudicate fairness.

### Reproducibility

Given the same images, model and metric, scores are exactly reproducible
— one of the method’s genuine advantages over mock-witness testing.
Record the model, the metric and the package versions, since a different
DeepFace release may ship different weights.

## Session Info

``` r

sessionInfo()

# Check Python environment
reticulate::py_config()
```
