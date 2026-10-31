# Compute Lineup Similarity Matrix

Computes similarity between a target face and all foils in a lineup,
returning a comprehensive similarity analysis useful for lineup fairness
assessment.

## Usage

``` r
lineup_similarity(
  target_path,
  foil_paths,
  model = "ArcFace",
  metric = "cosine",
  detector = "retinaface",
  foil_names = NULL
)
```

## Arguments

- target_path:

  Path to the target (suspect) face image.

- foil_paths:

  Character vector of paths to foil face images.

- model:

  Character. Face recognition model. Default is "ArcFace".

- metric:

  Character. Distance metric. Default is "cosine".

- detector:

  Character. Face detection backend. Default is "retinaface".

- foil_names:

  Optional character vector of names/labels for foils. If NULL, uses
  file names.

## Value

A tibble with columns:

- foil_id - Numeric ID (1, 2, 3, ...)

- foil_name - Name/label for the foil

- foil_path - Path to foil image

- distance - Distance from target (lower = more similar)

- similarity - Cosine similarity score \[-1,1\] for cosine metric

- verified - Would be identified as same person

- rank - Rank by similarity (1 = most similar to target)

Also includes attributes:

- target_path - Path to target image

- model - Model used

- metric - Metric used

- threshold - Verification threshold

## Details

This function describes how similar each foil is to the target in a face
recognition model's embedding space. It does not assess fairness: no
validated similarity range defines a fair lineup, and embedding
similarity is not the same as resemblance judged by people or as
mock-witness choice. Foils very dissimilar to the target may make the
target stand out; foils the model judges to be the same identity
(verified = TRUE) deserve inspection. Rows are ordered by rank (most
similar first), and the model's distance threshold is stored in
`attr(x, "threshold")`. For cosine, similarity = 1 - distance, which
ranges from -1 to 1.

## Examples

``` r
if (FALSE) { # \dontrun{
# Analyze a 6-person lineup
foils <- paste0("foil", 1:5, ".jpg")
lineup <- lineup_similarity("target.jpg", foils)

# View results sorted by similarity
print(lineup)

# Check which foils might be too similar
lineup[lineup$verified, ]

# Get summary statistics
summary(lineup)
} # }
```
