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
  "z"
))

# Determine how innocent-suspect identifications are represented in a
# target-absent dataset. If explicit suspect rows are present, they are the
# designated innocent-suspect responses and filler rows must not be added to
# them. Otherwise, filler choices are divided by nominal lineup size.
.innocent_suspect_method <- function(ta_data) {
  if (any(ta_data$identification == "suspect", na.rm = TRUE)) {
    "designated"
  } else {
    "estimated_from_fillers"
  }
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
