# This file contains print methods used in this package

my_sprintf <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}

my_sprintf_cat <- function(s, ...) {
  cat(sprintf(s, ...))
}

#' @title Output from a contingency tables method
#' @param x The output from a function from the [contingencytables] package
#' @param as_list Print the elements of `x` as a list
#' @param ... unused (kept for consistency with the generic [base::print()])
#' @rdname print
#' @export
print.contingencytables_result <- function(x, as_list = FALSE, ...) {
  if (as_list) {
    print(vapply(x, list, list(names(x))))
  } else {
    print_structure <- attr(x, "print_structure")
    switch(
      class(print_structure),
      "function"  = cat(print_structure()),
      "character" = cat(print_structure)
    )
  }
  invisible(x)
}
