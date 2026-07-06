# Get 468-Point Face Mesh Landmarks

Extracts dense facial landmarks using MediaPipe Face Mesh, which
provides 468 3D landmark points covering the entire face.

## Usage

``` r
get_face_landmarks(img_path, return_3d = TRUE)
```

## Arguments

- img_path:

  Path to the image file.

- return_3d:

  Logical. If TRUE, returns 3D coordinates (x, y, z). If FALSE, returns
  only 2D coordinates (x, y). Default is TRUE.

## Value

A list containing:

- landmarks - Matrix of landmark coordinates (468 x 2 or 468 x 3)

- face_detected - Logical indicating if a face was found

- image_width - Width of the input image

- image_height - Height of the input image

## Details

MediaPipe Face Mesh provides detailed landmarks covering:

- Face oval/contour

- Left and right eyebrows

- Left and right eyes (including iris)

- Nose

- Lips (inner and outer contours)

- Face interior regions (cheeks, forehead)

The x and y coordinates are normalized to \[0, 1\] relative to image
dimensions. The z coordinate represents depth relative to the face
center.

## Note

Requires MediaPipe to be installed (installed by default with
install_r4lineups_python()).

## Examples

``` r
if (FALSE) { # \dontrun{
# Get face mesh landmarks
mesh <- get_face_landmarks("face.jpg")

# Plot the landmarks
plot(mesh$landmarks[, 1], mesh$landmarks[, 2], pch = ".")
} # }
```
