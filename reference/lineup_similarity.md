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

- similarity - Similarity score \[0,1\] for cosine metric

- verified - Would be identified as same person

- rank - Rank by similarity (1 = most similar to target)

Also includes attributes:

- target_path - Path to target image

- model - Model used

- metric - Metric used

- threshold - Verification threshold

## Details

This is the primary function for assessing lineup fairness from a face
similarity perspective. Foils that are too similar to the target (low
distance, high similarity, verified=TRUE) may bias the lineup. Foils
that are too dissimilar may make the target stand out.

For a fair lineup, foils should have moderate similarity to the target,
ideally within a range that makes them plausible alternatives without
being too close to the target.

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
