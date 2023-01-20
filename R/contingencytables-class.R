# Instantiates an object of the "contingencytables_*" classes
newContingencytablesOutput <- function(content, class_name = "single") {
  # Defining valid structure
  valid_classes <- c(
    "contingencytables_singletest",
    "contingencytables_multipletests"
  )
  # Expanding shortened class names
  class_name <- switch(
    class_name,
    "single"   = valid_classes[1],
    "multiple" = valid_classes[2],
    class_name
  )
  valid_content <- switch(
    class_name,
    "contingencytables_singletest"   = c("name", "statistics"),
    "contingencytables_multipletests" = c("statistics", "FUN"),
    stop(
      "Invalid class_name. Please choose between ",
      paste(valid_classes, collapse = ", ")
    )
  )

  # Validating input
  names_match <- all(sort(names(content)) %in% sort(valid_content))
  if (is.null(names(content)) || !names_match) {
    stop(
      "Invalid content. List must contain the following elements: ",
      paste(valid_content, collapse = ", ")
    )
  } else {
    if (class_name == "contingencytables_multipletests") {
      FUN_arguments <- names(formals(content$FUN))
      stopifnot(FUN_arguments == "statistics")
    }
    return(structure(content, class = class_name))
  }
}

#' @title contingencytables_result class
#' @description A class for output of the main functions on this package
#' @param statistics Either a value or a list of values to be filled by
#' print_format
#' @param print_format Either a string of a function instructing how to print
#' the values from \code{statistics}
#' @param bundle Handles the legacy implementation using the
#' \code{contingencytables_singletest} class, where \code{bundle} is a list
#' containing the names "name" and "statistics"
#' @return an object of class \code{contingencytables_result}
#' @author Waldir Leoncio
contingencytables_result <- function(statistics, print_format, bundle = NULL) {
  if (!is.null(bundle)) {
    statistics <- bundle$statistics
    print_format <- fetch_print_format(bundle)
  }
  obj <- structure(
    list("statistics" = statistics, "print_format" = print_format),
    class = "contingencytables_result"
  )
  return(obj)
}

fetch_print_format <- function(x) {
  # Determining output format
  stats_names <- paste(names(x$statistics), collapse = "_")
  stats <- x$statistics
  out_stats <- switch(
    EXPR = stats_names,
    "lower_upper_estimate_alpha_statname" = sprintf(
      "%s = %6.4f (%g%% CI %6.4f to %6.4f)",
      stats$statname, stats$estimate, 100 * (1 - stats$alpha), stats$lower,
      stats$upper
    ),
    "pvalue_df_estimate_statname" = sprintf(
      "P = %8.6f, %s = %5.3f (df = %g)",
      stats$pvalue, stats$statname, stats$estimate, stats$df
    ),
    "pvalue_statname" = sprintf(
      "%s = %7.5f",
      stats$statname, stats$pvalue
    ),
    "lower_upper_deltahat" = sprintf(
      "  pi_%g+ vs pi_ + %g: delta = %7.4f (%7.4f to %7.4f)",
      seq_along(stats$deltahat), seq_along(stats$deltahat),
      stats$deltahat, stats$lower, stats$upper
    ),
    "lower_upper_differences_r" = sprintf(
      "  pi_1|%i - pi_1|%i: estimate = %6.4f (%6.4f to %6.4f)",
      pairwiseComparisons(stats$r)[, 1],
      pairwiseComparisons(stats$r)[, 2],
      stats$differences, stats$lower, stats$upper
    ),
    "t_pvalue" = sprintf(
      "T = %6.3f, P = %7.5f", stats$t, stats$pvalue
    )
  )
  # If out_stats is a vector of strings, each one would be on its own line
  # Example: all switch() cases above that involve seq_along()
  separator <- ifelse(length(out_stats) == 1, ": ", "\n")

  # Handling output with multiple names
  if (length(x$name) == 1) {
    printed_stats <- paste(out_stats, collapse = separator)
    if (length(printed_stats) == 1) {
      print_string <- paste(x$name, printed_stats, sep = ": ")
    } else {
      print_string <- paste(x$name, printed_stats, sep = "\n")
    }
  } else {
    print_string <- paste(paste0(x$name, ": ", out_stats), collapse = separator)
  }
  return(print_string)
}
