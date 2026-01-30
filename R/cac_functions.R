#' Compute CAC (Confidence-Accuracy Characteristic) Data
#'
#' Computes confidence-accuracy data for lineup identifications following
#' Mickes (2015). CAC analysis shows the relationship between confidence
#' and accuracy for suspect identifications.
#'
#' @param data A dataframe with the following columns:
#'   \itemize{
#'     \item target_present: Logical. TRUE if guilty suspect in lineup
#'     \item identification: Character. "suspect", "filler", or "reject"
#'     \item confidence: Numeric. Confidence rating
#'   }
#' @param lineup_size Integer. Number of people in lineup (default = 6)
#' @param confidence_bins Numeric vector of bin edges for grouping confidence
#'   (e.g., c(0, 60, 80, 100) creates bins 0-60, 61-80, 81-100).
#'   If NULL, uses individual confidence levels.
#'
#' @return A list containing:
#'   \itemize{
#'     \item cac_data: Dataframe with confidence levels and accuracy
#'     \item n_ids_per_level: Number of suspect IDs at each confidence level
#'     \item overall_accuracy: Overall accuracy across all suspect IDs
#'   }
#'
#' @details
#' CAC analysis computes, for each confidence level:
#' \deqn{Accuracy = \frac{Correct Suspect IDs}{Correct Suspect IDs + Incorrect Suspect IDs}}
#'
#' Only suspect IDs are included (filler IDs are ignored). For target-absent
#' lineups with no designated innocent suspect, filler IDs are divided by
#' lineup size to estimate incorrect suspect IDs.
#'
#' According to Mickes (2015), CAC analysis is most relevant for triers of fact
#' (judges/jurors) evaluating estimator variables (e.g., exposure duration,
#' retention interval). It shows how trustworthy an ID is at different confidence
#' levels.
#'
#' @references
#' Mickes, L. (2015). Receiver operating characteristic analysis and
#' confidence-accuracy characteristic analysis in investigations of system
#' variables and estimator variables that affect eyewitness memory.
#' \emph{Journal of Applied Research in Memory and Cognition, 4}(2), 93-102.
#'
#' Juslin, P., Olsson, N., & Winman, A. (1996). Calibration and diagnosticity
#' of confidence in eyewitness identification: Comments on what can be inferred
#' from the low confidence-accuracy correlation. \emph{Journal of Experimental
#' Psychology: Learning, Memory, and Cognition, 22}(5), 1304-1316.
#'
#' @export
#' @import tibble
make_cacdata <- function(data, lineup_size = 6, confidence_bins = NULL) {

  # Validate required columns
  required_cols <- c("target_present", "identification", "confidence")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Separate target-present and target-absent
  tp_data <- data[data$target_present == TRUE, ]
  ta_data <- data[data$target_present == FALSE, ]

  # Apply confidence binning if specified
  if (!is.null(confidence_bins)) {
    data$conf_binned <- cut(data$confidence,
                            breaks = confidence_bins,
                            include.lowest = TRUE,
                            right = TRUE)
    tp_data$conf_binned <- cut(tp_data$confidence,
                               breaks = confidence_bins,
                               include.lowest = TRUE,
                               right = TRUE)
    ta_data$conf_binned <- cut(ta_data$confidence,
                               breaks = confidence_bins,
                               include.lowest = TRUE,
                               right = TRUE)
    conf_var <- "conf_binned"
  } else {
    conf_var <- "confidence"
  }

  # Get unique confidence levels
  if (!is.null(confidence_bins)) {
    conf_levels <- levels(data$conf_binned)
  } else {
    conf_levels <- sort(unique(data$confidence), decreasing = FALSE)
  }

  # Initialize results
  cac_results <- tibble::tibble(
    confidence = character(),
    n_correct = numeric(),
    n_incorrect = numeric(),
    n_total = numeric(),
    accuracy = numeric(),
    se = numeric()
  )

  for (conf in conf_levels) {
    # Count correct suspect IDs (target-present, suspect chosen)
    if (!is.null(confidence_bins)) {
      n_correct <- sum(tp_data$identification == "suspect" &
                      tp_data$conf_binned == conf, na.rm = TRUE)
    } else {
      n_correct <- sum(tp_data$identification == "suspect" &
                      tp_data$confidence == conf, na.rm = TRUE)
    }

    # Count incorrect suspect IDs (target-absent, suspect chosen)
    if (!is.null(confidence_bins)) {
      n_incorrect_suspects <- sum(ta_data$identification == "suspect" &
                                  ta_data$conf_binned == conf, na.rm = TRUE)
      n_incorrect_fillers <- sum(ta_data$identification == "filler" &
                                ta_data$conf_binned == conf, na.rm = TRUE)
    } else {
      n_incorrect_suspects <- sum(ta_data$identification == "suspect" &
                                  ta_data$confidence == conf, na.rm = TRUE)
      n_incorrect_fillers <- sum(ta_data$identification == "filler" &
                                ta_data$confidence == conf, na.rm = TRUE)
    }

    # Estimate incorrect suspect IDs from fillers
    n_estimated_incorrect <- n_incorrect_fillers / lineup_size
    n_incorrect_total <- n_incorrect_suspects + n_estimated_incorrect

    # Calculate accuracy
    n_total <- n_correct + n_incorrect_total
    if (n_total > 0) {
      accuracy <- n_correct / n_total
      # Standard error (binomial)
      se <- sqrt((accuracy * (1 - accuracy)) / n_total)
    } else {
      accuracy <- NA
      se <- NA
    }

    cac_results <- rbind(cac_results, tibble::tibble(
      confidence = if (is.factor(conf)) as.character(conf) else as.character(conf),
      n_correct = n_correct,
      n_incorrect = n_incorrect_total,
      n_total = n_total,
      accuracy = accuracy,
      se = se
    ))
  }

  # Calculate overall accuracy
  total_correct <- sum(tp_data$identification == "suspect")
  total_incorrect_suspects <- sum(ta_data$identification == "suspect")
  total_incorrect_fillers <- sum(ta_data$identification == "filler")
  total_incorrect <- total_incorrect_suspects + (total_incorrect_fillers / lineup_size)
  overall_accuracy <- total_correct / (total_correct + total_incorrect)

  list(
    cac_data = cac_results,
    overall_accuracy = overall_accuracy,
    n_total_suspect_ids = total_correct + total_incorrect,
    lineup_size = lineup_size
  )
}


#' Plot CAC Curve
#'
#' Creates a ggplot2 visualization of CAC data showing the relationship
#' between confidence and accuracy.
#'
#' @param cacobj_list List output from make_cacdata()
#' @param show_errorbars Logical. Whether to show error bars (default = TRUE)
#' @param show_n Logical. Whether to show sample sizes (default = TRUE)
#'
#' @return A ggplot2 object
#'
#' @export
#' @import ggplot2
make_cac_gg <- function(cacobj_list, show_errorbars = TRUE, show_n = TRUE) {

  cac_data <- cacobj_list$cac_data

  # Create numeric x-axis for plotting
  cac_data$conf_numeric <- 1:nrow(cac_data)

  # Base plot
  p <- ggplot(cac_data, aes(x = conf_numeric, y = accuracy)) +
    geom_line(size = 1, color = "darkgreen") +
    geom_point(size = 3, color = "darkgreen") +
    theme_bw(base_size = 14) +
    scale_x_continuous(
      breaks = 1:nrow(cac_data),
      labels = cac_data$confidence
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    labs(
      x = "Confidence Level",
      y = "Proportion Correct (Suspect IDs Only)",
      title = "CAC: Confidence-Accuracy Characteristic",
      caption = "Accuracy = Correct Suspect IDs / (Correct + Incorrect Suspect IDs)"
    ) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50")

  # Add error bars
  if (show_errorbars) {
    p <- p + geom_errorbar(
      aes(ymin = accuracy - se, ymax = accuracy + se),
      width = 0.2,
      color = "darkgreen"
    )
  }

  # Add sample size labels
  if (show_n) {
    p <- p + geom_text(
      aes(label = paste0("n=", round(n_total))),
      nudge_y = -0.08,
      size = 3,
      color = "gray30"
    )
  }

  p
}


#' Compute and Plot CAC for Lineup Identification
#'
#' Main function to compute and plot CAC (Confidence-Accuracy Characteristic)
#' for eyewitness lineup data.
#'
#' @param data A dataframe with columns: target_present, identification, confidence
#' @param lineup_size Integer. Number of people in lineup (default = 6)
#' @param confidence_bins Numeric vector of bin edges (optional)
#' @param show_plot Logical. Whether to display the plot (default = TRUE)
#' @param ... Additional arguments passed to make_cac_gg()
#'
#' @return A list containing CAC data and plot
#'
#' @examples
#' \dontrun{
#' # Example with binned confidence
#' cac_result <- make_cac(lineup_data, confidence_bins = c(0, 60, 80, 100))
#' print(cac_result)
#' }
#'
#' @export
make_cac <- function(data, lineup_size = 6, confidence_bins = NULL,
                     show_plot = TRUE, ...) {

  # Compute CAC data
  cac_obj <- make_cacdata(data, lineup_size = lineup_size,
                          confidence_bins = confidence_bins)

  # Create plot
  if (show_plot) {
    cac_plot <- make_cac_gg(cac_obj, ...)
  } else {
    cac_plot <- NULL
  }

  result <- list(
    plot = cac_plot,
    cac_data = cac_obj$cac_data,
    overall_accuracy = cac_obj$overall_accuracy,
    n_total_suspect_ids = cac_obj$n_total_suspect_ids,
    lineup_size = cac_obj$lineup_size
  )

  class(result) <- c("lineup_cac", "list")
  result
}


#' Print Method for lineup_cac Objects
#' @param x A lineup_cac object
#' @param ... Additional arguments (ignored)
#' @export
print.lineup_cac <- function(x, ...) {
  cat("\n=== Lineup CAC Analysis ===\n\n")
  cat("Overall Accuracy:", round(x$overall_accuracy, 3), "\n")
  cat("Total Suspect IDs:", round(x$n_total_suspect_ids), "\n")
  cat("Lineup size:", x$lineup_size, "\n\n")

  cat("CAC Data:\n")
  print(x$cac_data, n = Inf)

  if (!is.null(x$plot)) {
    cat("\nPlot available in $plot\n")
  }

  invisible(x)
}
