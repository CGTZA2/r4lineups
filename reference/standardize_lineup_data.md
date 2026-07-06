# Standardize Lineup Identification Data

Converts lineup data from various formats into the standardized
r4lineups format. Handles common data issues and ensures compatibility
with all confidence-based analyses.

## Usage

``` r
standardize_lineup_data(
  data,
  target_present_col = "target_present",
  identification_col = "identification",
  confidence_col = "confidence",
  response_time_col = NULL,
  participant_id_col = NULL,
  recode_identification = NULL,
  validate = TRUE
)
```

## Arguments

- data:

  A data frame with lineup identification data

- target_present_col:

  Name of column indicating target presence (default =
  "target_present"). Can also be "tp", "culprit_present", etc.

- identification_col:

  Name of column with identification responses (default =
  "identification"). Can also be "response", "choice", etc.

- confidence_col:

  Name of column with confidence ratings (default = "confidence"). Can
  also be "conf", "certainty", etc.

- response_time_col:

  Name of column with response times (optional)

- participant_id_col:

  Name of column with participant IDs (optional)

- recode_identification:

  Named character vector for recoding identification values. E.g.,
  c("target" = "suspect", "foil" = "filler")

- validate:

  Logical. Whether to validate the result (default = TRUE)

## Value

A standardized data frame with class "lineup_data" containing:

- target_present: Logical

- identification: Character ("suspect", "filler", "reject")

- confidence: Numeric (if present)

- participant_id: Original or generated (if present)

- response_time: Numeric (if present)

## Details

This function:

- Renames columns to standard names

- Converts data types as needed

- Recodes identification values to standard terms

- Adds participant IDs if missing

- Validates the result (if validate = TRUE)

- Adds S3 class "lineup_data"

\*\*Common recoding patterns:\*\*

- "target" → "suspect"

- "foil" / "distractor" → "filler"

- "none" / "not present" / "no choice" → "reject"

## Examples

``` r
# Data with non-standard column names
raw_data <- data.frame(
  tp = c(1, 1, 0, 0),
  response = c("target", "foil", "none", "target"),
  conf = c(5, 3, 2, 4)
)

# Standardize
std_data <- standardize_lineup_data(
  raw_data,
  target_present_col = "tp",
  identification_col = "response",
  confidence_col = "conf",
  recode_identification = c(
    "target" = "suspect",
    "foil" = "filler",
    "none" = "reject"
  )
)
#> Warning: Very small sample size (n=4). Results may be unreliable.
print(std_data)
#> 
#> === Standardized Lineup Data ===
#> 
#> Format: r4lineups standard format
#> Rows: 4 
#> Columns: target_present, identification, confidence, participant_id 
#> 
#> Target-present trials: 2 
#> Target-absent trials: 2 
#> 
#> Identifications:
#>   filler    : 1
#>   reject    : 1
#>   suspect   : 2
#> 
#> Confidence range: 2 to 5 
#> 
#> First 10 rows:
#>   target_present identification confidence participant_id
#> 1           TRUE        suspect          5              1
#> 2           TRUE         filler          3              2
#> 3          FALSE         reject          2              3
#> 4          FALSE        suspect          4              4
```
