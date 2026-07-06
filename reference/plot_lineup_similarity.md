# Plot Lineup Similarity Results

Creates a bar plot showing the similarity of each foil to the target
face, useful for visualizing lineup fairness.

## Usage

``` r
plot_lineup_similarity(
  lineup,
  show_threshold = TRUE,
  use_similarity = TRUE,
  title = NULL,
  colors = NULL
)
```

## Arguments

- lineup:

  A lineup_similarity object from
  [`lineup_similarity`](https://cgtza2.github.io/r4lineups/reference/lineup_similarity.md).

- show_threshold:

  Logical. If TRUE, adds a horizontal line at the verification
  threshold. Default is TRUE.

- use_similarity:

  Logical. If TRUE, plots similarity scores (higher = more similar). If
  FALSE, plots distance (lower = more similar). Default is TRUE.

- title:

  Optional plot title.

- colors:

  Color palette for bars. Default uses a gradient from blue (dissimilar)
  to red (similar).

## Value

A ggplot2 object.

## Examples

``` r
if (FALSE) { # \dontrun{
lineup <- lineup_similarity("target.jpg", foil_paths)
plot_lineup_similarity(lineup)

# Show distance instead of similarity
plot_lineup_similarity(lineup, use_similarity = FALSE)
} # }
```
