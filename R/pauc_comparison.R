#' Compare pAUC Between Two ROC Curves
#'
#' Statistically compares partial Area Under the Curve (pAUC) values between
#' two lineup conditions using bootstrap-based standard errors and z-tests.
#' Follows the methodology from pyWitness (Mickes et al., 2024).
#'
#' @param data1 First dataset (dataframe with target_present, identification, confidence)
#' @param data2 Second dataset (same format as data1)
#' @param lineup_size Integer. Number of people in lineup (default = 6)
#' @param max_false_id_rate Numeric. Maximum false ID rate for pAUC calculation (default = NULL,
#'   which uses the maximum observed false ID rate across both conditions)
#' @param n_bootstrap Integer. Number of bootstrap samples (default = 2000)
#' @param conf_level Numeric. Confidence level for intervals (default = 0.95)
#' @param seed Integer. Random seed for reproducibility (default = NULL)
#' @param label1 Character. Label for first condition (default = "Condition 1")
#' @param label2 Character. Label for second condition (default = "Condition 2")
#'
#' @return An object of class "pauc_comparison" containing:
#'   \itemize{
#'     \item pauc1: pAUC for condition 1
#'     \item pauc2: pAUC for condition 2
#'     \item pauc_diff: Difference in pAUC (pauc1 - pauc2)
#'     \item se_pauc1: Bootstrap standard error for pAUC1
#'     \item se_pauc2: Bootstrap standard error for pAUC2
#'     \item se_diff: Standard error of the difference
#'     \item z_score: Z-statistic for difference test
#'     \item p_value: Two-tailed p-value
#'     \item ci_diff: Confidence interval for difference
#'     \item roc1: ROC object for condition 1
#'     \item roc2: ROC object for condition 2
#'     \item max_false_id_rate: Cutoff used for pAUC
#'     \item label1, label2: Condition labels
#'     \item n_bootstrap: Number of bootstrap samples used
#'   }
#'
#' @details
#' This function implements a rigorous statistical test for comparing ROC curves
#' between two conditions (e.g., simultaneous vs. sequential lineups, different
#' retention intervals, etc.).
#'
#' **Method:**
#' \enumerate{
#'   \item Computes pAUC for each condition (optionally up to a maximum false ID rate)
#'   \item Uses bootstrap resampling to estimate standard errors
#'   \item Computes Z-statistic: Z = (pAUC1 - pAUC2) / SE(pAUC1 - pAUC2)
#'   \item Calculates p-value from standard normal distribution
#' }
#'
#' **Interpretation:**
#' \itemize{
#'   \item Positive pAUC difference: Condition 1 has better discriminability
#'   \item Negative pAUC difference: Condition 2 has better discriminability
#'   \item p < 0.05: Significant difference between conditions
#' }
#'
#' @references
#' Mickes, L., Seale-Carlisle, T. M., Chen, X., & Boogert, S. (2024). pyWitness 1.0:
#' A python eyewitness identification analysis toolkit. \emph{Behavior Research
#' Methods, 56}, 1533-1550.
#'
#' Wixted, J. T., & Mickes, L. (2012). The field of eyewitness memory should
#' abandon probative value and embrace receiver operating characteristic analysis.
#' \emph{Perspectives on Psychological Science, 7}(3), 275-278.
#'
#' @examples
#' \dontrun{
#' # Compare simultaneous vs. sequential lineups
#' comparison <- compare_pauc(
#'   simultaneous_data,
#'   sequential_data,
#'   label1 = "Simultaneous",
#'   label2 = "Sequential",
#'   n_bootstrap = 2000
#' )
#'
#' print(comparison)
#' plot(comparison)
#' }
#'
#' @export
#' @importFrom stats qnorm pnorm
compare_pauc <- function(data1,
                         data2,
                         lineup_size = 6,
                         max_false_id_rate = NULL,
                         n_bootstrap = 2000,
                         conf_level = 0.95,
                         seed = NULL,
                         label1 = "Condition 1",
                         label2 = "Condition 2") {

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Compute ROC for both conditions
  roc1 <- make_rocdata(data1, lineup_size = lineup_size)
  roc2 <- make_rocdata(data2, lineup_size = lineup_size)

  # Determine max false ID rate cutoff
  if (is.null(max_false_id_rate)) {
    max_false_id_rate <- max(
      max(roc1$roc_data$false_id_rate),
      max(roc2$roc_data$false_id_rate)
    )
  }

  # Compute pAUC for both conditions (up to cutoff)
  pauc1 <- .compute_pauc_with_cutoff(roc1$roc_data, max_false_id_rate)
  pauc2 <- .compute_pauc_with_cutoff(roc2$roc_data, max_false_id_rate)

  # Bootstrap to estimate standard errors
  message("Computing bootstrap standard errors (", n_bootstrap, " samples)...")
  bootstrap_results <- .bootstrap_pauc_comparison(
    data1, data2,
    lineup_size = lineup_size,
    max_false_id_rate = max_false_id_rate,
    n_bootstrap = n_bootstrap
  )

  # Standard errors
  se_pauc1 <- stats::sd(bootstrap_results$pauc1_boot)
  se_pauc2 <- stats::sd(bootstrap_results$pauc2_boot)
  se_diff <- stats::sd(bootstrap_results$diff_boot)

  # Z-test for difference
  pauc_diff <- pauc1 - pauc2
  z_score <- pauc_diff / se_diff
  p_value <- 2 * (1 - stats::pnorm(abs(z_score)))  # Two-tailed

  # Confidence interval for difference
  z_crit <- stats::qnorm(1 - (1 - conf_level) / 2)
  ci_lower <- pauc_diff - z_crit * se_diff
  ci_upper <- pauc_diff + z_crit * se_diff

  # Create result object
  result <- list(
    pauc1 = pauc1,
    pauc2 = pauc2,
    pauc_diff = pauc_diff,
    se_pauc1 = se_pauc1,
    se_pauc2 = se_pauc2,
    se_diff = se_diff,
    z_score = z_score,
    p_value = p_value,
    ci_diff = c(lower = ci_lower, upper = ci_upper),
    roc1 = roc1,
    roc2 = roc2,
    max_false_id_rate = max_false_id_rate,
    label1 = label1,
    label2 = label2,
    n_bootstrap = n_bootstrap,
    conf_level = conf_level,
    bootstrap_results = bootstrap_results
  )

  class(result) <- c("pauc_comparison", "list")
  message("Done!")
  return(result)
}


#' Compute pAUC with False ID Rate Cutoff
#'
#' @param roc_data Dataframe with roc data (from make_rocdata)
#' @param max_false_id_rate Maximum false ID rate cutoff
#'
#' @return Numeric pAUC value
#' @keywords internal
.compute_pauc_with_cutoff <- function(roc_data, max_false_id_rate) {

  # Filter to points at or below cutoff
  roc_filtered <- roc_data[roc_data$false_id_rate <= max_false_id_rate, ]

  # If no points at cutoff, interpolate
  if (nrow(roc_filtered) == 0 || max(roc_filtered$false_id_rate) < max_false_id_rate) {
    # Find points immediately below and above cutoff
    below_idx <- which(roc_data$false_id_rate <= max_false_id_rate)
    above_idx <- which(roc_data$false_id_rate > max_false_id_rate)

    if (length(below_idx) > 0 && length(above_idx) > 0) {
      # Get nearest points
      below_pt <- roc_data[max(below_idx), ]
      above_pt <- roc_data[min(above_idx), ]

      # Linear interpolation
      slope <- (above_pt$correct_id_rate - below_pt$correct_id_rate) /
               (above_pt$false_id_rate - below_pt$false_id_rate)
      interpolated_correct_rate <- below_pt$correct_id_rate +
        slope * (max_false_id_rate - below_pt$false_id_rate)

      # Add interpolated point
      interp_pt <- data.frame(
        confidence = NA,
        correct_id_rate = interpolated_correct_rate,
        false_id_rate = max_false_id_rate,
        n_correct_ids = NA,
        n_false_ids = NA
      )

      roc_filtered <- rbind(roc_filtered, interp_pt)
    }
  }

  # Sort by false ID rate
  roc_filtered <- roc_filtered[order(roc_filtered$false_id_rate), ]

  # Compute AUC using trapezoidal rule
  if (nrow(roc_filtered) < 2) {
    return(0)
  }

  pauc <- 0
  for (i in 2:nrow(roc_filtered)) {
    width <- roc_filtered$false_id_rate[i] - roc_filtered$false_id_rate[i-1]
    height <- (roc_filtered$correct_id_rate[i] + roc_filtered$correct_id_rate[i-1]) / 2
    pauc <- pauc + (width * height)
  }

  return(pauc)
}


#' Bootstrap pAUC Comparison
#'
#' @param data1 First dataset
#' @param data2 Second dataset
#' @param lineup_size Lineup size
#' @param max_false_id_rate Maximum false ID rate
#' @param n_bootstrap Number of bootstrap samples
#'
#' @return List with bootstrap distributions
#' @keywords internal
.bootstrap_pauc_comparison <- function(data1, data2, lineup_size,
                                       max_false_id_rate, n_bootstrap) {

  n1 <- nrow(data1)
  n2 <- nrow(data2)

  pauc1_boot <- numeric(n_bootstrap)
  pauc2_boot <- numeric(n_bootstrap)
  diff_boot <- numeric(n_bootstrap)

  for (b in 1:n_bootstrap) {
    # Resample with replacement
    boot_idx1 <- sample(1:n1, n1, replace = TRUE)
    boot_idx2 <- sample(1:n2, n2, replace = TRUE)

    boot_data1 <- data1[boot_idx1, ]
    boot_data2 <- data2[boot_idx2, ]

    # Compute ROC and pAUC for bootstrap samples
    tryCatch({
      boot_roc1 <- make_rocdata(boot_data1, lineup_size = lineup_size)
      boot_roc2 <- make_rocdata(boot_data2, lineup_size = lineup_size)

      pauc1_boot[b] <- .compute_pauc_with_cutoff(boot_roc1$roc_data, max_false_id_rate)
      pauc2_boot[b] <- .compute_pauc_with_cutoff(boot_roc2$roc_data, max_false_id_rate)
      diff_boot[b] <- pauc1_boot[b] - pauc2_boot[b]
    }, error = function(e) {
      # If bootstrap sample fails, use NA
      pauc1_boot[b] <<- NA
      pauc2_boot[b] <<- NA
      diff_boot[b] <<- NA
    })
  }

  # Remove any NAs
  valid_idx <- !is.na(pauc1_boot) & !is.na(pauc2_boot)
  pauc1_boot <- pauc1_boot[valid_idx]
  pauc2_boot <- pauc2_boot[valid_idx]
  diff_boot <- diff_boot[valid_idx]

  list(
    pauc1_boot = pauc1_boot,
    pauc2_boot = pauc2_boot,
    diff_boot = diff_boot
  )
}


#' Print Method for pauc_comparison Objects
#'
#' @param x A pauc_comparison object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.pauc_comparison <- function(x, ...) {
  cat("\n=== pAUC Comparison Analysis ===\n\n")

  cat("Conditions:\n")
  cat("  ", x$label1, ": pAUC = ", round(x$pauc1, 4), " (SE = ", round(x$se_pauc1, 4), ")\n", sep = "")
  cat("  ", x$label2, ": pAUC = ", round(x$pauc2, 4), " (SE = ", round(x$se_pauc2, 4), ")\n", sep = "")
  cat("\n")

  cat("Difference: ", round(x$pauc_diff, 4), "\n", sep = "")
  cat("  95% CI: [", round(x$ci_diff["lower"], 4), ", ", round(x$ci_diff["upper"], 4), "]\n", sep = "")
  cat("\n")

  cat("Statistical Test:\n")
  cat("  Z = ", round(x$z_score, 3), "\n", sep = "")
  cat("  p-value = ", format.pval(x$p_value, digits = 4), "\n", sep = "")
  cat("\n")

  # Interpretation
  if (x$p_value < 0.001) {
    sig_text <- "***"
  } else if (x$p_value < 0.01) {
    sig_text <- "**"
  } else if (x$p_value < 0.05) {
    sig_text <- "*"
  } else {
    sig_text <- "ns"
  }

  cat("Interpretation: ")
  if (x$pauc_diff > 0) {
    cat(x$label1, " has higher discriminability than ", x$label2, " (", sig_text, ")\n", sep = "")
  } else if (x$pauc_diff < 0) {
    cat(x$label2, " has higher discriminability than ", x$label1, " (", sig_text, ")\n", sep = "")
  } else {
    cat("No difference in discriminability (", sig_text, ")\n", sep = "")
  }

  cat("\n")
  cat("Max false ID rate cutoff: ", round(x$max_false_id_rate, 4), "\n", sep = "")
  cat("Bootstrap samples: ", x$n_bootstrap, "\n", sep = "")
  cat("\nNote: *** p<0.001, ** p<0.01, * p<0.05, ns = not significant\n")

  invisible(x)
}


#' Summary Method for pauc_comparison Objects
#'
#' @param object A pauc_comparison object
#' @param ... Additional arguments (ignored)
#'
#' @export
summary.pauc_comparison <- function(object, ...) {
  cat("\n=== pAUC Comparison Summary ===\n\n")

  # Condition details
  cat("Condition 1:", object$label1, "\n")
  cat("  Sample size:", object$roc1$n_target_present + object$roc1$n_target_absent, "\n")
  cat("  Target-present:", object$roc1$n_target_present, "\n")
  cat("  Target-absent:", object$roc1$n_target_absent, "\n")
  cat("  pAUC:", round(object$pauc1, 4), "\n")
  cat("  SE(pAUC):", round(object$se_pauc1, 4), "\n")
  cat("  95% CI: [", round(object$pauc1 - 1.96*object$se_pauc1, 4), ", ",
      round(object$pauc1 + 1.96*object$se_pauc1, 4), "]\n\n", sep = "")

  cat("Condition 2:", object$label2, "\n")
  cat("  Sample size:", object$roc2$n_target_present + object$roc2$n_target_absent, "\n")
  cat("  Target-present:", object$roc2$n_target_present, "\n")
  cat("  Target-absent:", object$roc2$n_target_absent, "\n")
  cat("  pAUC:", round(object$pauc2, 4), "\n")
  cat("  SE(pAUC):", round(object$se_pauc2, 4), "\n")
  cat("  95% CI: [", round(object$pauc2 - 1.96*object$se_pauc2, 4), ", ",
      round(object$pauc2 + 1.96*object$se_pauc2, 4), "]\n\n", sep = "")

  # Comparison
  cat("Difference (", object$label1, " - ", object$label2, "):\n", sep = "")
  cat("  Estimate:", round(object$pauc_diff, 4), "\n")
  cat("  SE(diff):", round(object$se_diff, 4), "\n")
  cat("  95% CI: [", round(object$ci_diff["lower"], 4), ", ",
      round(object$ci_diff["upper"], 4), "]\n", sep = "")
  cat("  Z-score:", round(object$z_score, 3), "\n")
  cat("  p-value:", format.pval(object$p_value, digits = 4), "\n\n")

  # Effect size (Cohen's d equivalent for pAUC)
  pooled_se <- sqrt((object$se_pauc1^2 + object$se_pauc2^2) / 2)
  effect_size <- object$pauc_diff / pooled_se
  cat("Effect size (d):", round(effect_size, 3), "\n")

  # Power calculation (approximate)
  cat("\nAnalysis Details:\n")
  cat("  Max false ID rate cutoff:", round(object$max_false_id_rate, 4), "\n")
  cat("  Bootstrap samples:", object$n_bootstrap, "\n")
  cat("  Confidence level:", object$conf_level * 100, "%\n", sep = "")

  invisible(object)
}


#' Plot pAUC Comparison
#'
#' Creates a side-by-side visualization of ROC curves for two conditions
#' with shaded pAUC regions and statistical test results.
#'
#' @param x A pauc_comparison object from compare_pauc()
#' @param show_cutoff Logical. Whether to show false ID rate cutoff line (default = TRUE)
#' @param show_test_results Logical. Whether to show test results on plot (default = TRUE)
#' @param ... Additional arguments (ignored)
#'
#' @return A ggplot2 object
#'
#' @details
#' Creates a comparison plot showing:
#' \itemize{
#'   \item ROC curves for both conditions
#'   \item Shaded pAUC regions (up to cutoff)
#'   \item Vertical line at false ID rate cutoff
#'   \item Test statistics (Z-score, p-value)
#' }
#'
#' @export
#' @import ggplot2
plot.pauc_comparison <- function(x, show_cutoff = TRUE, show_test_results = TRUE, ...) {

  # Combine ROC data with condition labels
  roc1_data <- x$roc1$roc_data
  roc1_data$condition <- x$label1

  roc2_data <- x$roc2$roc_data
  roc2_data$condition <- x$label2

  combined_data <- rbind(roc1_data, roc2_data)

  # Create base plot
  p <- ggplot(combined_data, aes(x = false_id_rate, y = correct_id_rate,
                                  color = condition, fill = condition)) +
    geom_line(size = 1.2) +
    geom_point(shape = 21, size = 3, color = "black", aes(fill = condition)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    theme_bw(base_size = 14) +
    labs(
      x = "False ID Rate (Innocent Suspect)",
      y = "Correct ID Rate (Guilty Suspect)",
      title = "pAUC Comparison Between Conditions",
      color = "Condition",
      fill = "Condition"
    ) +
    scale_color_manual(values = c("#e41a1c", "#377eb8")) +
    scale_fill_manual(values = c("#e41a1c", "#377eb8")) +
    theme(legend.position = "top")

  # Add shaded pAUC regions (up to cutoff)
  roc1_filtered <- roc1_data[roc1_data$false_id_rate <= x$max_false_id_rate, ]
  roc2_filtered <- roc2_data[roc2_data$false_id_rate <= x$max_false_id_rate, ]

  if (nrow(roc1_filtered) > 1) {
    p <- p + geom_ribbon(
      data = roc1_filtered,
      aes(ymin = false_id_rate, ymax = correct_id_rate),
      alpha = 0.2,
      color = NA
    )
  }

  if (nrow(roc2_filtered) > 1) {
    p <- p + geom_ribbon(
      data = roc2_filtered,
      aes(ymin = false_id_rate, ymax = correct_id_rate),
      alpha = 0.2,
      color = NA
    )
  }

  # Add cutoff line
  if (show_cutoff) {
    p <- p + geom_vline(
      xintercept = x$max_false_id_rate,
      linetype = "dotted",
      color = "gray30",
      size = 1
    ) +
    annotate(
      "text",
      x = x$max_false_id_rate,
      y = 0.05,
      label = paste0("Cutoff = ", round(x$max_false_id_rate, 3)),
      hjust = -0.1,
      size = 3,
      color = "gray30"
    )
  }

  # Add test results
  if (show_test_results) {
    sig_stars <- ifelse(x$p_value < 0.001, "***",
                       ifelse(x$p_value < 0.01, "**",
                             ifelse(x$p_value < 0.05, "*", "ns")))

    test_text <- paste0(
      "Δ pAUC = ", round(x$pauc_diff, 4), "\n",
      "Z = ", round(x$z_score, 2), ", ",
      "p = ", format.pval(x$p_value, digits = 3), " ", sig_stars
    )

    p <- p + annotate(
      "text",
      x = max(combined_data$false_id_rate) * 0.7,
      y = 0.15,
      label = test_text,
      size = 4,
      hjust = 0,
      vjust = 0,
      fontface = "bold"
    )
  }

  p
}
