# This file contains print methods used in this package

my_sprintf <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}

#' @export
print.contingencytables_output <- function(x, ...) {
  cat(
    x$name,
    sprintf(
      ": estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
      x$estimate, 100 * (1 - x$alpha), x$lower, x$upper
    ),
    sep = ""
  )
  invisible(x)
}
