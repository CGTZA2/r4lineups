# Validate Lineup Identification Data

Checks whether a data frame meets the requirements for r4lineups
confidence-based analyses. Ensures required columns exist with correct
types and valid values.

## Usage

``` r
validate_lineup_data(
  data,
  require_confidence = TRUE,
  require_response_time = FALSE,
  strict = FALSE
)
```

## Arguments

- data:

  A data frame to validate

- require_confidence:

  Logical. Whether confidence column is required (default = TRUE). Set
  to FALSE for analyses that don't need confidence.

- require_response_time:

  Logical. Whether response_time is required (default = FALSE)

- strict:

  Logical. If TRUE, stops with error on validation failure. If FALSE,
  returns validation result with messages (default = FALSE)

## Value

If strict = FALSE, returns a list with:

- valid: Logical indicating if data passes validation

- messages: Character vector of validation messages/errors

- warnings: Character vector of warnings (non-fatal issues)

If strict = TRUE, either returns TRUE invisibly or stops with error.

## Details

\*\*Required columns:\*\*

- \`target_present\`: Logical (TRUE/FALSE) indicating if target in
  lineup

- \`identification\`: Character or factor with values "suspect",
  "filler", or "reject"

- \`confidence\`: Numeric (if require_confidence = TRUE). Higher = more
  confident

\*\*Optional columns:\*\*

- \`participant_id\`: Unique identifier for each trial/participant

- \`response_time\`: Numeric response time in milliseconds

\*\*Validation checks:\*\*

- All required columns present

- Correct data types

- No missing values in required columns

- Valid identification values

- Positive confidence values

- At least some target-present and target-absent trials

## Examples

``` r
# Valid data
valid_data <- data.frame(
  target_present = c(TRUE, TRUE, FALSE, FALSE),
  identification = c("suspect", "filler", "reject", "suspect"),
  confidence = c(5, 3, 2, 4)
)
validate_lineup_data(valid_data)
#> $valid
#> [1] TRUE
#> 
#> $messages
#> [1] "Data is valid"
#> 
#> $warnings
#> [1] "Very small sample size (n=4). Results may be unreliable."
#> 

# Invalid data (missing column)
invalid_data <- data.frame(
  target_present = c(TRUE, FALSE),
  identification = c("suspect", "filler")
)
result <- validate_lineup_data(invalid_data, strict = FALSE)
print(result$messages)
#> [1] "Missing required columns: confidence"

# Strict validation (errors on failure)
validate_lineup_data(valid_data, strict = TRUE)
#> Warning: Very small sample size (n=4). Results may be unreliable.
```
