#' Example Lineup Identification Data
#'
#' A dataset containing simulated eyewitness lineup identification data with
#' target-present and target-absent lineups, including confidence ratings.
#'
#' @format A data frame with 200 rows and 3 variables:
#' \describe{
#'   \item{target_present}{Logical indicating whether the guilty suspect was in the lineup}
#'   \item{identification}{Character indicating the witness decision: "suspect", "filler", or "reject"}
#'   \item{confidence}{Numeric confidence rating (0-100 scale)}
#' }
#'
#' @details
#' This dataset is used for demonstrating ROC, CAC, and EIG analyses in the
#' r4lineups package. It contains 100 target-present lineups and 100 target-absent
#' lineups with realistic distributions of identification decisions and confidence
#' ratings.
#'
#' @source Simulated data for package examples
#' @examples
#' data(lineup_example)
#' str(lineup_example)
#' table(lineup_example$identification, lineup_example$target_present)
"lineup_example"
