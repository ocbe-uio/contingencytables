#' @title contingencytables_result class
#' @description A class for output of the main functions on this package
#' @param statistics Either a value or a list of values to be filled by
#' print_format
#' @param print_structure Either a string of a function instructing how to print
#' the values from `statistics`
#' @return an object of class `contingencytables_result`
#' @seealso [print.contingencytables_result]
#' @author Waldir Leoncio
contingencytables_result <- function(statistics, print_structure) {
  obj <- structure(statistics, class = "contingencytables_result")
  attr(obj, "print_structure") <- print_structure
  attr(obj, "row.names") <- NULL # workaround for data frames
  return(obj)
}

#' @title Fetch print format
#' @description This is just a way of automating the retrieval of common
#' @param x a named vector, data.frame or list containing test statistics
#' @return A string with the proper print_structure for `x`
#' printing formats for the contingencytables_result class
fetch_print_format <- function(x) { # TODO: supersede
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
