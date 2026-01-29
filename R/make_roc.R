#' Compute and Plot ROC Curve for Lineup Identification
#'
#' Main function to compute and plot an ROC curve for eyewitness lineup data.
#' This follows the methodology of Wixted & Mickes (2012) and Mickes (2015).
#'
#' @param data A dataframe with the following columns:
#'   \itemize{
#'     \item target_present: Logical. TRUE if guilty suspect in lineup
#'     \item identification: Character. "suspect", "filler", or "reject"
#'     \item confidence: Numeric. Confidence rating
#'   }
#' @param lineup_size Integer. Number of people in lineup (default = 6)
#' @param show_plot Logical. Whether to display the plot (default = TRUE)
#' @param ... Additional arguments passed to make_roc_gg()
#'
#' @return A list containing:
#'   \itemize{
#'     \item plot: ggplot2 object (if show_plot = TRUE)
#'     \item roc_data: Dataframe with ROC curve points
#'     \item pauc: Partial area under the curve
#'     \item summary: Summary statistics
#'   }
#'
#' @details
#' ROC analysis measures discriminability - the ability to distinguish innocent
#' from guilty suspects. According to Mickes (2015), ROC analysis is most relevant
#' for policymakers deciding on system variables (e.g., simultaneous vs. sequential
#' lineups, lineup size, etc.).
#'
#' For estimator variables (e.g., exposure duration, retention interval), use
#' CAC analysis instead (see \code{\link{make_cac}}).
#'
#' @examples
#' \dontrun{
#' # Example data structure:
#' lineup_data <- data.frame(
#'   target_present = c(TRUE, TRUE, FALSE, FALSE),
#'   identification = c("suspect", "filler", "suspect", "reject"),
#'   confidence = c(90, 70, 80, 50)
#' )
#'
#' # Compute and plot ROC
#' roc_result <- make_roc(lineup_data)
#' print(roc_result$pauc)
#' }
#'
#' @references
#' Wixted, J. T., & Mickes, L. (2012). The field of eyewitness memory should
#' abandon probative value and embrace receiver operating characteristic analysis.
#' \emph{Perspectives on Psychological Science, 7}(3), 275-278.
#'
#' Mickes, L. (2015). Receiver operating characteristic analysis and
#' confidence-accuracy characteristic analysis in investigations of system
#' variables and estimator variables that affect eyewitness memory.
#' \emph{Journal of Applied Research in Memory and Cognition, 4}(2), 93-102.
#'
#' @export
make_roc <- function(data, lineup_size = 6, show_plot = TRUE, ...) {

  # Compute ROC data
  roc_obj <- make_rocdata(data, lineup_size = lineup_size)

  # Create plot
  if (show_plot) {
    roc_plot <- make_roc_gg(roc_obj, ...)
  } else {
    roc_plot <- NULL
  }

  # Create summary
  summary_stats <- list(
    pauc = roc_obj$pauc,
    n_target_present = roc_obj$n_target_present,
    n_target_absent = roc_obj$n_target_absent,
    lineup_size = roc_obj$lineup_size,
    n_confidence_levels = nrow(roc_obj$roc_data) - 1,  # Exclude (0,0) point
    max_correct_id_rate = max(roc_obj$roc_data$correct_id_rate),
    max_false_id_rate = max(roc_obj$roc_data$false_id_rate)
  )

  result <- list(
    plot = roc_plot,
    roc_data = roc_obj$roc_data,
    pauc = roc_obj$pauc,
    summary = summary_stats
  )

  class(result) <- c("lineup_roc", "list")
  result
}


#' Print Method for lineup_roc Objects
#' @param x A lineup_roc object
#' @param ... Additional arguments (ignored)
#' @export
print.lineup_roc <- function(x, ...) {
  cat("\n=== Lineup ROC Analysis ===\n\n")
  cat("Partial AUC:", round(x$pauc, 3), "\n")
  cat("Target-present lineups:", x$summary$n_target_present, "\n")
  cat("Target-absent lineups:", x$summary$n_target_absent, "\n")
  cat("Lineup size:", x$summary$lineup_size, "\n")
  cat("Confidence levels:", x$summary$n_confidence_levels, "\n\n")

  cat("ROC Data:\n")
  print(x$roc_data, n = Inf)

  if (!is.null(x$plot)) {
    cat("\nPlot available in $plot\n")
  }

  invisible(x)
}
