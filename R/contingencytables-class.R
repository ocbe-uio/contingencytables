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
