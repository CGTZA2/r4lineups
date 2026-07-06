# Plot Similarity Distribution

Creates a histogram or density plot showing the distribution of face
similarities, useful for understanding the range of similarities in a
lineup or database.

## Usage

``` r
plot_similarity_distribution(
  similarities,
  type = c("histogram", "density"),
  threshold = NULL,
  xlab = "Similarity",
  title = "Distribution of Face Similarities"
)
```

## Arguments

- similarities:

  Numeric vector of similarity scores or distances.

- type:

  Plot type: "histogram" or "density".

- threshold:

  Optional threshold value to show as vertical line.

- xlab:

  X-axis label.

- title:

  Plot title.

## Value

A ggplot2 object.

## Examples

``` r
set.seed(123)
similarities <- rbeta(100, 2, 5)
plot_similarity_distribution(similarities, threshold = 0.6)

```
