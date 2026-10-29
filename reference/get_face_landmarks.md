# Get 478-Point Face Mesh Landmarks

Extracts dense facial landmarks using MediaPipe Face Mesh, which
provides 478 3D landmark points covering the entire face, including the
irises.

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

- landmarks - Matrix of landmark coordinates (478 x 2 or 478 x 3)

- face_detected - Logical indicating if a face was found

- image_width - Width of the input image

- image_height - Height of the input image

- n_landmarks - Number of landmarks (when a face was found)

## Details

MediaPipe Face Mesh provides detailed landmarks covering:

- Face oval/contour

- Left and right eyebrows

- Left and right eyes, with 10 iris points (468 to 477)

- Nose

- Lips (inner and outer contours)

- Face interior regions (cheeks, forehead)

The x and y coordinates are normalized to \[0, 1\] relative to image
dimensions. The z coordinate represents depth relative to the face
center.

Older MediaPipe releases expose the legacy
`mediapipe.solutions.face_mesh` interface, which is used when present.
Current releases have only the Tasks API (`FaceLandmarker`), which needs
a model file. It is downloaded once, on first use, to
`tools::R_user_dir("r4lineups", "cache")`. On macOS the Tasks API runs
on the GPU delegate, because the CPU delegate in current macOS wheels
aborts the process.

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
