#' @keywords internal
#' @importFrom stats density
#' @importFrom utils globalVariables
utils::globalVariables(c(
  # ggplot2 aesthetics used in plotting functions
  "Dim1",
  "Dim2",
  "accuracy",
  "anri",
  "base_rate",
  "ci_lower",
  "ci_upper",
  "condition",
  "conf_numeric",
  "confidence",
  "correct_id_rate",
  "correction",
  "criterion",
  "cumulative_false_alarm_rate",
  "cumulative_hit_rate",
  "delta",
  "difference",
  "distance",
  "effective_size",
  "error_rate",
  "evidence_direction",
  "evidence_label",
  "expected",
  "expected_utility",
  "false_id_rate",
  "fsmax",
  "foil_name",
  "group",
  "highlighted",
  "identification",
  "information_gain",
  "label",
  "model",
  "mean_confidence_plot",
  "n",
  "n_total",
  "observed",
  "posterior",
  "posterior_guilty",
  "pearson_chisq",
  "ppv",
  "ppv_nominal",
  "ppv_none",
  "prior",
  "prior_a",
  "procedure",
  "response",
  "response_cat",
  "response_label",
  "se",
  "source",
  "similarity",
  "ss",
  "stats_label",
  "cell",
  "count",
  "converged",
  "false_alarm_rate",
  "hit_rate",
  "target_present",
  "z",
  # multid plots
  "component",
  "end",
  "estimate",
  "kind",
  "pos",
  "start",
  "threshold",
  "value"
))

# Determine how innocent-suspect identifications are represented in a
# target-absent dataset. With a designated innocent suspect, the explicit
# suspect rows are the innocent-suspect responses and filler rows must not be
# added to them. Without one, filler choices are divided by nominal lineup size.
#
# `innocent_suspect = "auto"` treats the lineups as having a designated suspect
# when any target-absent row is a suspect ID or the data carry a non-missing
# `suspect_pos` column for target-absent rows. A designated suspect who is never
# chosen is still designated, so zero suspect IDs alone is not evidence that
# there was none. Resolved values ("designated", "estimated_from_fillers") pass
# through unchanged, so a bootstrap can resolve once on the full data and reuse
# the result in every replicate.
.innocent_suspect_method <- function(ta_data, innocent_suspect = "auto") {
  if (!is.character(innocent_suspect) || length(innocent_suspect) != 1L ||
      !innocent_suspect %in% c("auto", "designated", "estimated",
                               "estimated_from_fillers")) {
    stop("innocent_suspect must be one of \"auto\", \"designated\" or ",
         "\"estimated\".", call. = FALSE)
  }
  if (innocent_suspect == "designated") return("designated")
  if (innocent_suspect %in% c("estimated", "estimated_from_fillers")) {
    return("estimated_from_fillers")
  }
  has_suspect_id <- any(ta_data$identification == "suspect", na.rm = TRUE)
  has_suspect_pos <- "suspect_pos" %in% names(ta_data) &&
    any(!is.na(ta_data$suspect_pos))
  if (has_suspect_id || has_suspect_pos) {
    return("designated")
  }
  message("No target-absent suspect IDs and no suspect_pos column; treating ",
          "the lineups as having no designated innocent suspect and estimating ",
          "innocent-suspect IDs as filler IDs / lineup_size. Set ",
          "innocent_suspect = \"designated\" if a suspect was designated but ",
          "never chosen.")
  "estimated_from_fillers"
}

# Resolve the innocent-suspect design once on a full dataset, so that resampled
# or subsetted analyses all use the same design.
.resolve_innocent_suspect <- function(data, innocent_suspect = "auto") {
  .innocent_suspect_method(
    data[!as.logical(data$target_present), , drop = FALSE], innocent_suspect
  )
}

.innocent_suspect_count <- function(ta_data, include, lineup_size,
                                    method = .innocent_suspect_method(ta_data)) {
  if (!is.numeric(lineup_size) || length(lineup_size) != 1L ||
      is.na(lineup_size) || lineup_size < 1) {
    stop("lineup_size must be a positive number.", call. = FALSE)
  }
  include <- as.logical(include)
  if (length(include) != nrow(ta_data) || anyNA(include)) {
    stop("Internal inclusion mask does not match target-absent data.", call. = FALSE)
  }
  if (method == "designated") {
    sum(include & ta_data$identification == "suspect", na.rm = TRUE)
  } else {
    sum(include & ta_data$identification == "filler", na.rm = TRUE) / lineup_size
  }
}

# Shared validation for analytical entry points. This intentionally avoids the
# sample-size warnings produced by strict validation.
.validate_lineup_analysis <- function(data, require_confidence = TRUE,
                                      require_response_time = FALSE) {
  checked <- validate_lineup_data(
    data,
    require_confidence = require_confidence,
    require_response_time = require_response_time,
    strict = FALSE
  )
  if (!isTRUE(checked$valid)) {
    stop(paste(checked$messages, collapse = "; "), call. = FALSE)
  }
  tp <- as.logical(data$target_present)
  if (!any(tp) || !any(!tp)) {
    stop("Data must include both target-present and target-absent trials.",
         call. = FALSE)
  }
  invisible(TRUE)
}

.adjust_innocent_response_counts <- function(counts, lineup_size, method) {
  counts <- as.numeric(counts) |> stats::setNames(names(counts))
  if (method == "designated") return(counts)
  adjusted <- numeric()
  add_count <- function(name, value) {
    current <- adjusted[name]
    if (length(current) == 0L || is.na(current)) current <- 0
    adjusted[name] <<- current + value
  }
  for (response in names(counts)) {
    count <- unname(counts[[response]])
    if (response == "filler" || startsWith(response, "filler_")) {
      suspect_response <- sub("^filler", "suspect", response)
      add_count(suspect_response, count / lineup_size)
      add_count(response, count * (lineup_size - 1) / lineup_size)
    } else {
      add_count(response, count)
    }
  }
  adjusted
}

.local_seed <- function(seed) {
  if (is.null(seed)) return(function() invisible(NULL))
  seed_existed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (seed_existed) get(".Random.seed", envir = .GlobalEnv) else NULL
  set.seed(seed)
  function() {
    if (seed_existed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
    invisible(NULL)
  }
}
