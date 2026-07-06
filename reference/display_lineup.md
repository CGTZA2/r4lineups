# Display Lineup as Image Grid

Creates a visual display of the lineup faces arranged in a grid.

## Usage

``` r
display_lineup(
  target_path,
  foil_paths,
  ncol = 3,
  scale = 150,
  show_labels = TRUE,
  highlight_target = TRUE
)
```

## Arguments

- target_path:

  Path to target image.

- foil_paths:

  Paths to foil images.

- ncol:

  Number of columns in the grid.

- scale:

  Scale factor for images.

- show_labels:

  Logical. Add labels under each face.

- highlight_target:

  Logical. Add border around target.

## Value

A magick image object that can be displayed or saved.

## Examples

``` r
if (FALSE) { # \dontrun{
# Display lineup
img <- display_lineup("target.jpg", paste0("foil", 1:5, ".jpg"))
print(img)

# Save to file
magick::image_write(img, "lineup_display.png")
} # }
```
