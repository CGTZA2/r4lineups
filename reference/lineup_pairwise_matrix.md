# Compute Pairwise Similarity Matrix for All Lineup Members

Computes a full pairwise similarity matrix between all faces in a
lineup, including the target. Useful for understanding the overall
structure of face similarities in the lineup.

## Usage

``` r
lineup_pairwise_matrix(
  target_path,
  foil_paths,
  model = "ArcFace",
  metric = "cosine",
  detector = "retinaface"
)
```

## Arguments

- target_path:

  Path to target face.

- foil_paths:

  Paths to foil faces.

- model:

  Face recognition model.

- metric:

  Distance metric.

- detector:

  Face detector.

## Value

A square matrix of pairwise distances, with row/column names.

## Examples

``` r
if (FALSE) { # \dontrun{
mat <- lineup_pairwise_matrix("target.jpg", c("foil1.jpg", "foil2.jpg"))
# Visualize with heatmap
heatmap(mat)
} # }
```
