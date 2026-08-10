#' Compute ROC Data for Lineup Identification
#'
#' Computes Receiver Operating Characteristic (ROC) data from lineup identification
#' experiments following the methodology of Mickes, Wixted, and Gronlund.
#'
#' @param data A dataframe with the following columns:
#'   \itemize{
#'     \item target_present: Logical. TRUE if guilty suspect in lineup, FALSE if innocent
#'     \item identification: Character. "suspect", "filler", or "reject"
#'     \item confidence: Numeric. Confidence rating (higher = more confident)
#'   }
#' @param lineup_size Integer. Number of people in the lineup (default = 6).
#'   Used to estimate false ID rate when fillers are chosen from target-absent lineups.
#'
#' @return A list containing:
#'   \itemize{
#'     \item roc_data: Dataframe with correct_id_rate, false_id_rate, and confidence
#'     \item pauc: Partial area under the curve
#'     \item n_target_present: Number of target-present lineups
#'     \item n_target_absent: Number of target-absent lineups
#'   }
#'
#' @details
#' This function computes ROC curves following Wixted & Mickes (2012) and
#' Mickes (2015). For each confidence level:
#' \itemize{
#'   \item Correct ID rate = proportion of target-present lineups where suspect identified
#'   \item False ID rate = proportion of target-absent lineups where suspect identified
#' }
#'
#' If target-absent data contain explicit \code{"suspect"} responses, those
#' designated innocent-suspect IDs are used directly. Otherwise, filler IDs
#' are divided by lineup size. The two estimators are never added together.
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
#' Gronlund, S. D., Wixted, J. T., & Mickes, L. (2014). Evaluating eyewitness
#' identification procedures using receiver operating characteristic analysis.
#' \emph{Current Directions in Psychological Science, 23}(1), 3-10.
#'
#' @examples
#' data(lineup_example)
#' roc <- make_rocdata(lineup_example)
#' roc$roc_data
#' roc$pauc
#'
#' @export
#' @import tibble
make_rocdata <- function(data, lineup_size = 6) {
  .validate_lineup_analysis(data)

  # Validate required columns
  required_cols <- c("target_present", "identification", "confidence")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Separate target-present and target-absent lineups
  tp_data <- data[data$target_present == TRUE, ]
  ta_data <- data[data$target_present == FALSE, ]

  n_tp <- nrow(tp_data)
  n_ta <- nrow(ta_data)

  if (n_tp == 0 | n_ta == 0) {
    stop("Data must include both target-present and target-absent lineups")
  }

  # Get unique confidence levels (sorted from high to low)
  conf_levels <- sort(unique(data$confidence), decreasing = TRUE)

  # Initialize results
  roc_results <- tibble::tibble(
    confidence = numeric(),
    correct_id_rate = numeric(),
    false_id_rate = numeric(),
    n_correct_ids = numeric(),
    n_false_ids = numeric()
  )

  # For each confidence level (cumulative from high to low)
  for (conf in conf_levels) {
    # Target-present: count suspect IDs at or above this confidence
    n_correct <- sum(tp_data$identification == "suspect" & tp_data$confidence >= conf)
    correct_id_rate <- n_correct / n_tp

    method <- .innocent_suspect_method(ta_data)
    n_false <- .innocent_suspect_count(
      ta_data, ta_data$confidence >= conf, lineup_size, method
    )
    false_id_rate <- n_false / n_ta

    roc_results <- rbind(roc_results, tibble::tibble(
      confidence = conf,
      correct_id_rate = correct_id_rate,
      false_id_rate = false_id_rate,
      n_correct_ids = n_correct,
      n_false_ids = n_false
    ))
  }

  # Add point at (0, 0) for reject all
  roc_results <- rbind(tibble::tibble(
    confidence = min(conf_levels) - 1,
    correct_id_rate = 0,
    false_id_rate = 0,
    n_correct_ids = 0,
    n_false_ids = 0
  ), roc_results)

  # Calculate partial AUC (using trapezoidal rule)
  # Sort by false_id_rate for integration
  roc_sorted <- roc_results[order(roc_results$false_id_rate), ]

  pauc <- 0
  for (i in 2:nrow(roc_sorted)) {
    width <- roc_sorted$false_id_rate[i] - roc_sorted$false_id_rate[i-1]
    height <- (roc_sorted$correct_id_rate[i] + roc_sorted$correct_id_rate[i-1]) / 2
    pauc <- pauc + (width * height)
  }

  list(
    roc_data = roc_results,
    pauc = pauc,
    n_target_present = n_tp,
    n_target_absent = n_ta,
    lineup_size = lineup_size,
    innocent_suspect_method = method,
    raw_data = data
  )
}


#' Plot ROC Curve for Lineup Identification
#'
#' Creates a ggplot2 visualization of ROC data from lineup identification experiments.
#'
#' @param rocobj_list List output from make_rocdata()
#' @param show_pauc Logical. Whether to display pAUC value on plot (default = TRUE)
#' @param point_labels Logical. Whether to label points with confidence levels (default = TRUE)
#'
#' @return A ggplot2 object
#'
#' @examples
#' data(lineup_example)
#' roc <- make_rocdata(lineup_example)
#' make_roc_gg(roc)
#'
#' @export
#' @import ggplot2 ggrepel
make_roc_gg <- function(rocobj_list, show_pauc = TRUE, point_labels = TRUE) {

  roc_data <- rocobj_list$roc_data
  pauc <- rocobj_list$pauc

  # Create base plot
  p <- ggplot(roc_data, aes(x = false_id_rate, y = correct_id_rate)) +
    geom_line(linewidth = 1, color = "steelblue") +
    geom_point(shape = 21, color = "black", fill = "white", size = 3) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    theme_bw(base_size = 14) +
    labs(
      x = "False ID Rate (Innocent Suspect)",
      y = "Correct ID Rate (Guilty Suspect)",
      title = "ROC Curve for Lineup Identification",
      caption = "Dashed line represents chance performance"
    ) +
    coord_cartesian(xlim = c(0, max(roc_data$false_id_rate) * 1.1),
                    ylim = c(0, 1))

  # Add confidence labels
  if (point_labels) {
    p <- p + geom_text_repel(
      aes(label = round(confidence, 1)),
      nudge_x = 0.01,
      nudge_y = 0.02,
      size = 3
    )
  }

  # Add shaded area under curve
  p <- p + geom_ribbon(
    aes(ymin = 0, ymax = correct_id_rate),
    alpha = 0.2,
    fill = "steelblue"
  )

  # Add pAUC annotation
  if (show_pauc) {
    p <- p + annotate(
      "text",
      x = max(roc_data$false_id_rate) * 0.7,
      y = 0.1,
      label = paste0("pAUC = ", round(pauc, 3)),
      size = 5
    )
  }

  p
}
