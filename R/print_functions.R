# This file contains print methods used in this package

my_sprintf <- function(s, ...) {
  # TODO: once #31 is merged, this can be superseded by my_sprintf_cat()
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}

my_sprintf_cat <- function(s, ...) {
  cat(sprintf(s, ...))
}

#' @export
print.contingencytables_singletest <- function(x, ...) {
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
    "lower_upper_differences" = sprintf(
      "  pi_1|%i - pi_1|%i: estimate = %6.4f (%6.4f to %6.4f)",
      pairwiseComparisons(length(stats$differences) - 1)[, 1],
      pairwiseComparisons(length(stats$differences) - 1)[, 2],
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
    cat(x$name, out_stats, sep = separator)
  } else {
    cat(paste0(x$name, ": ", out_stats), sep = separator)
  }
  invisible(x)
}

#' @export
print.contingencytables_multipletests <- function(x, ...) {
  x$FUN(x$statistics)
  invisible(x)
}
