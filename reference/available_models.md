# List Available Face Recognition Models

Returns a character vector of available face recognition models that can
be used for embedding extraction.

## Usage

``` r
available_models()
```

## Value

Character vector of model names.

## Details

Available models with their embedding dimensions:

- VGG-Face (4096-d) - Legacy model

- Facenet (128-d) - Google's FaceNet

- Facenet512 (512-d) - FaceNet with larger embeddings

- OpenFace (128-d) - Open source

- DeepFace (4096-d) - Facebook's model

- DeepID (160-d) - Compact model

- ArcFace (512-d) - State-of-the-art, recommended

- Dlib (128-d) - Lightweight

- SFace (128-d) - Lightweight

- GhostFaceNet (512-d) - Efficient

## Examples

``` r
available_models()
#>  [1] "VGG-Face"     "Facenet"      "Facenet512"   "OpenFace"     "DeepFace"    
#>  [6] "DeepID"       "ArcFace"      "Dlib"         "SFace"        "GhostFaceNet"
```
