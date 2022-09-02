# This file contains print methods used in this package

my_sprintf <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}

#' @export
print.contingencytables_output <- function(x, ...) {
  # Determining output format
  stats_names <- paste(names(x$statistics), collapse = "_")
  stats <- x$statistics
  out_stats <- switch(
    EXPR = stats_names,
    "lower_upper_estimate_alpha" = sprintf(
      "estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
      stats$estimate, 100 * (1 - stats$alpha), stats$lower, stats$upper
    ),
    "pvalue_df_estimate" = sprintf(
      "P = %8.6f, T = %6.3f (df=%g)",
      stats$pvalue, stats$estimate, stats$df
    ),
    "pvalue" = sprintf(
      "P = %7.5f",
      stats$pvalue
    )
  )
  cat(x$name, out_stats, sep = ": ")
  invisible(x)
}
