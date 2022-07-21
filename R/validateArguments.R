#' @title Validate arguments of a function
#' @description This is an internal function used by user-level functions to
#' validate their arguments.
#' @param x named list containing function arguments and their values
#' @param types named vector of types for \code{x}
#' @return Nothing if all aguments fit their type. An error message otherwise.
#' @author Waldir Leoncio
#' @export
#' @details Accepted validation types are:
#' \itemize{
#'  \item "counts"
#'  \item "positive"
#'  \item "probability"
#'  \item "linear, log or logit"
#'  \item "MH or IV"
#'  \item
#' }
#' @note Types are evaluated alphabetically, and errors accuse no more than
#' one invalid argument at a time.
#' @examples
#' Adjusted_inv_sinh_CI_OR_2x2(ritland_2007)
#' \dontrun{Adjusted_inv_sinh_CI_OR_2x2(-ritland_2007)}
#'
validateArguments <- function(x, types = "default") {
  for (i in seq_along(x)) {
    if (length(types) == 1 && types == "default") {
      type <- switch(
        names(x)[i],
        "n" = "counts",
        "alpha" = "probability",
        "psi1" = "positive",
        "psi2" = "positive",
        "pi0" = "probability",
        "link" = "linear, log or logit",
        "estimatetype" = "MH or IV",
        "linkfunction" = "logit or probit",
        "skip"
      )
    } else {
      # This function is usually called with x = mget(ls()), which orders the
      # arguments alphabetically. Therefore, the types vector is also ordered
      # so the indices match.
      type <- types[order(names(types))][i]
    }
    isvalid <- switch(
      type,
      "counts" = all(trunc(x[[i]]) == x[[i]]) && all(x[[i]] >= 0),
      "probability"  = (x[[i]] >= 0) && (x[[i]] <= 1),
      "positive" = (x[[i]] > 0),
      "linear, log or logit" = x[[i]] %in% c("linear", "log", "logit"),
      "MH or IV" = x[[i]] %in% c("MH", "IV"),
      "logit or probit" = x[[i]] %in% c("logit", "probit"),
      "skip" = TRUE
    )
    if (!isvalid) {
      stop(
        names(x)[i], " contains invalid values. Should be ", type,
        ". Please read the function documentation for more information.",
        call. = FALSE
      )
    }
  }
}
