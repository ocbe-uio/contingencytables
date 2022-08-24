# This file contains print methods used in this package

my_sprintf <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
