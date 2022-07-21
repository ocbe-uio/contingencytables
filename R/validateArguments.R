#' @title Validate arguments of a function
#' @description This is an internal function used by user-level functions to
#' validate their arguments.
#' @param x named list containing function arguments and their values
#' @param types vector of types for \code{x}
#' @return Nothing if all aguments fit their type. An error message otherwise.
#' @author Waldir Leoncio
#' @export
#' @details Accepted validation types are:
#' \itemize{
#'  \item "count"
#'  \item "positive"
#'  \item "prob"
#' }
#' @examples
#' Adjusted_inv_sinh_CI_OR_2x2(ritland_2007)
#' \dontrun{Adjusted_inv_sinh_CI_OR_2x2(-ritland_2007)}
#'
validateArguments <- function(x, types = "default") {
  for (i in seq_along(x)) {
    if (types == "default") {
      type <- switch(
        names(x)[i],
        "n" = "counts",
        "alpha" = "prob",
        "psi1" = "positive",
        "psi2" = "positive",
        "skip"
      )
    } else {
      type <- types[i]
    }
    isvalid <- switch(
      type,
      "counts" = all(trunc(x[[i]]) == x[[i]]) && all(x[[i]] >= 0),
      "prob"  = (x[[i]] >= 0) && (x[[i]] <= 1),
      "positive" = (x[[i]] > 0),
      "skip" = TRUE
    )
    if (!isvalid) {
      stop(x[[i]], " is not a valid value for ", names(x)[i])
    } # TODO: improve feedback
  }
}
