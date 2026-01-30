#' Compute RAC (Response Time-Accuracy Characteristic) Data
#'
#' Computes response time-accuracy data for lineup identifications following
#' the approach described in Seale-Carlisle et al. (2019) and implemented in
#' pyWitness (Mickes et al., 2024). RAC analysis shows the relationship between
#' response time and accuracy for suspect identifications.
#'
#' @param data A dataframe with the following columns:
#'   \itemize{
#'     \item target_present: Logical. TRUE if guilty suspect in lineup
#'     \item identification: Character. "suspect", "filler", or "reject"
#'     \item response_time: Numeric. Response time in milliseconds (or seconds)
#'   }
#' @param lineup_size Integer. Number of people in lineup (default = 6)
#' @param time_bins Numeric vector of bin edges for grouping response times
#'   (e.g., c(0, 5000, 10000, 15000, 20000) creates bins in milliseconds).
#'   If NULL, uses individual response time values (not recommended for
#'   continuous data).
#'
#' @return A list containing:
#'   \itemize{
#'     \item rac_data: Dataframe with response time bins and accuracy
#'     \item n_ids_per_bin: Number of suspect IDs in each time bin
#'     \item overall_accuracy: Overall accuracy across all suspect IDs
#'   }
#'
#' @details
#' RAC analysis computes, for each response time bin:
#' \deqn{Accuracy = \frac{Correct Suspect IDs}{Correct Suspect IDs + Incorrect Suspect IDs}}
#'
#' Only suspect IDs are included (filler IDs and rejections are ignored).
#' For target-absent lineups with no designated innocent suspect, filler IDs
#' are divided by lineup size to estimate incorrect suspect IDs.
#'
#' RAC analysis is useful for examining the speed-accuracy tradeoff in
#' eyewitness identifications. Faster responses typically indicate stronger
#' memory, and RAC curves often show higher accuracy for faster response times.
#'
#' @references
#' Seale-Carlisle, T. M., Colloff, M. F., Flowe, H. D., Wells, W., Wixted, J. T.,
#' & Mickes, L. (2019). Confidence and response time as indicators of eyewitness
#' identification accuracy in the lab and in the real world. \emph{Journal of
#' Applied Research in Memory and Cognition, 8}(4), 420-428.
#'
#' Mickes, L., Seale-Carlisle, T. M., Chen, X., & Boogert, S. (2024).
#' pyWitness 1.0: A python eyewitness identification analysis toolkit.
#' \emph{Behavior Research Methods, 56}, 1533-1550.
#'
#' @export
#' @import tibble
make_racdata <- function(data, lineup_size = 6, time_bins = NULL) {

  # Validate required columns
  required_cols <- c("target_present", "identification", "response_time")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Separate target-present and target-absent
  tp_data <- data[data$target_present == TRUE, ]
  ta_data <- data[data$target_present == FALSE, ]

  # Apply response time binning
  if (!is.null(time_bins)) {
    data$time_binned <- cut(data$response_time,
                            breaks = time_bins,
                            include.lowest = TRUE,
                            right = TRUE)
    tp_data$time_binned <- cut(tp_data$response_time,
                               breaks = time_bins,
                               include.lowest = TRUE,
                               right = TRUE)
    ta_data$time_binned <- cut(ta_data$response_time,
                               breaks = time_bins,
                               include.lowest = TRUE,
                               right = TRUE)
    time_var <- "time_binned"
  } else {
    time_var <- "response_time"
  }

  # Get unique time levels
  if (!is.null(time_bins)) {
    time_levels <- levels(data$time_binned)
  } else {
    time_levels <- sort(unique(data$response_time), decreasing = FALSE)
  }

  # Initialize results
  rac_results <- tibble::tibble(
    response_time = character(),
    mean_time = numeric(),
    n_correct = numeric(),
    n_incorrect = numeric(),
    n_total = numeric(),
    accuracy = numeric(),
    se = numeric()
  )

  for (time_level in time_levels) {
    # Count correct suspect IDs (target-present, suspect chosen)
    if (!is.null(time_bins)) {
      n_correct <- sum(tp_data$identification == "suspect" &
                      tp_data$time_binned == time_level, na.rm = TRUE)
      # Calculate mean time in this bin
      mean_time <- mean(tp_data$response_time[
        tp_data$identification == "suspect" &
        tp_data$time_binned == time_level], na.rm = TRUE)
    } else {
      n_correct <- sum(tp_data$identification == "suspect" &
                      tp_data$response_time == time_level, na.rm = TRUE)
      mean_time <- time_level
    }

    # Count incorrect suspect IDs (target-absent, suspect chosen)
    if (!is.null(time_bins)) {
      n_incorrect_suspects <- sum(ta_data$identification == "suspect" &
                                  ta_data$time_binned == time_level, na.rm = TRUE)
      n_incorrect_fillers <- sum(ta_data$identification == "filler" &
                                ta_data$time_binned == time_level, na.rm = TRUE)
    } else {
      n_incorrect_suspects <- sum(ta_data$identification == "suspect" &
                                  ta_data$response_time == time_level, na.rm = TRUE)
      n_incorrect_fillers <- sum(ta_data$identification == "filler" &
                                ta_data$response_time == time_level, na.rm = TRUE)
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

    rac_results <- rbind(rac_results, tibble::tibble(
      response_time = if (is.factor(time_level)) as.character(time_level) else as.character(time_level),
      mean_time = mean_time,
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
    rac_data = rac_results,
    overall_accuracy = overall_accuracy,
    n_total_suspect_ids = total_correct + total_incorrect,
    lineup_size = lineup_size
  )
}


#' Plot RAC Curve
#'
#' Creates a ggplot2 visualization of RAC data showing the relationship
#' between response time and accuracy.
#'
#' @param racobj_list List output from make_racdata()
#' @param show_errorbars Logical. Whether to show error bars (default = TRUE)
#' @param show_n Logical. Whether to show sample sizes (default = TRUE)
#' @param time_units Character. Label for time units (e.g., "ms", "seconds").
#'   Default = "ms"
#'
#' @return A ggplot2 object
#'
#' @export
#' @import ggplot2
make_rac_gg <- function(racobj_list, show_errorbars = TRUE, show_n = TRUE,
                        time_units = "ms") {

  rac_data <- racobj_list$rac_data

  # Create numeric x-axis for plotting
  rac_data$time_numeric <- 1:nrow(rac_data)

  # Base plot
  p <- ggplot(rac_data, aes(x = time_numeric, y = accuracy)) +
    geom_line(size = 1, color = "steelblue") +
    geom_point(size = 3, color = "steelblue") +
    theme_bw(base_size = 14) +
    scale_x_continuous(
      breaks = 1:nrow(rac_data),
      labels = rac_data$response_time
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    labs(
      x = paste0("Response Time (", time_units, ")"),
      y = "Proportion Correct (Suspect IDs Only)",
      title = "RAC: Response Time-Accuracy Characteristic",
      caption = "Accuracy = Correct Suspect IDs / (Correct + Incorrect Suspect IDs)"
    ) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Add error bars
  if (show_errorbars) {
    p <- p + geom_errorbar(
      aes(ymin = accuracy - se, ymax = accuracy + se),
      width = 0.2,
      color = "steelblue"
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


#' Compute and Plot RAC for Lineup Identification
#'
#' Main function to compute and plot RAC (Response Time-Accuracy Characteristic)
#' for eyewitness lineup data.
#'
#' @param data A dataframe with columns: target_present, identification, response_time
#' @param lineup_size Integer. Number of people in lineup (default = 6)
#' @param time_bins Numeric vector of bin edges (recommended for continuous RT data)
#' @param show_plot Logical. Whether to display the plot (default = TRUE)
#' @param ... Additional arguments passed to make_rac_gg()
#'
#' @return A list containing RAC data and plot
#'
#' @examples
#' \dontrun{
#' # Example with binned response times (in milliseconds)
#' rac_result <- make_rac(lineup_data,
#'                        time_bins = c(0, 5000, 10000, 15000, 20000, 99999))
#' print(rac_result)
#' rac_result$plot
#' }
#'
#' @export
make_rac <- function(data, lineup_size = 6, time_bins = NULL,
                     show_plot = TRUE, ...) {

  # Compute RAC data
  rac_obj <- make_racdata(data, lineup_size = lineup_size,
                          time_bins = time_bins)

  # Create plot
  if (show_plot) {
    rac_plot <- make_rac_gg(rac_obj, ...)
  } else {
    rac_plot <- NULL
  }

  result <- list(
    plot = rac_plot,
    rac_data = rac_obj$rac_data,
    overall_accuracy = rac_obj$overall_accuracy,
    n_total_suspect_ids = rac_obj$n_total_suspect_ids,
    lineup_size = rac_obj$lineup_size
  )

  class(result) <- c("lineup_rac", "list")
  result
}


#' Print Method for lineup_rac Objects
#' @param x A lineup_rac object
#' @param ... Additional arguments (ignored)
#' @export
print.lineup_rac <- function(x, ...) {
  cat("\n=== Lineup RAC Analysis ===\n\n")
  cat("Overall Accuracy:", round(x$overall_accuracy, 3), "\n")
  cat("Total Suspect IDs:", round(x$n_total_suspect_ids), "\n")
  cat("Lineup size:", x$lineup_size, "\n\n")

  cat("RAC Data:\n")
  print(x$rac_data, n = Inf)

  if (!is.null(x$plot)) {
    cat("\nPlot available in $plot\n")
  }

  invisible(x)
}
