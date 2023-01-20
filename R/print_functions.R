# This file contains print methods used in this package

my_sprintf <- function(s, ...) {
  # TODO: once #31 is merged, this can be superseded by my_sprintf_cat()
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}

my_sprintf_cat <- function(s, ...) {
  cat(sprintf(s, ...))
}

#' @export
print.contingencytables_result <- function(x, ...) {
  if (is(x$print_format, "function")) {
    cat(x$print_format())
  } else {
    cat(x$print_format)
  }
  invisible(x)
}
