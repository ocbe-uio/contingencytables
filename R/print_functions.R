# This file contains print methods used in this package

cat_sprintf <- function(s, ...) {
  cat(sprintf(s, ...))
}

format_pvalue <- function(x, digits = 6, fixed_digits = NULL) {
  if (!is.numeric(x)) {
    return("NA")
  }
  if (!is.null(fixed_digits) && all(is.finite(x) & x != 0 & abs(x) >= 0.5 * 10^(-fixed_digits))) {
    return(sprintf(paste0("%.", fixed_digits, "f"), x))
  }
  format.pval(x, digits = digits, eps = .Machine$double.xmin)
}

sprintf <- function(fmt, ...) {
  args <- list(...)
  if (length(fmt) == 1L && length(args) > 0L) {
    matches <- gregexpr("(?<!%)%[-+ #0]*[0-9]*(?:\\.[0-9]+)?[a-zA-Z]", fmt, perl = TRUE)[[1]]
    if (matches[1] > 0L) {
      lengths <- attr(matches, "match.length")
      previous_end <- 1L
      pvalue_arguments <- integer()
      pvalue_formats <- character()
      for (i in seq_along(matches)) {
        conversion <- substr(fmt, matches[i], matches[i] + lengths[i] - 1L)
        before <- substr(fmt, previous_end, matches[i] - 1L)
        is_pvalue <- grepl("(?:P|mid-?P)\\s*=\\s*$", before, ignore.case = TRUE)
        if (i <= length(args) && is_pvalue && grepl("f$", conversion, ignore.case = TRUE) && is.numeric(args[[i]])) {
          decimals <- regmatches(conversion, regexpr("(?<=\\.)[0-9]+", conversion, perl = TRUE))
          if (length(decimals) == 0L || identical(decimals, character(0))) {
            decimals <- "6"
          }
          cutoff <- 0.5 * 10^(-as.numeric(decimals))
          if (any(args[[i]] == 0 | abs(args[[i]]) < cutoff, na.rm = TRUE)) {
            width <- sub("^%[-+ #0]*([0-9]*).*$", "\\1", conversion)
            pvalue_arguments <- c(pvalue_arguments, i)
            pvalue_formats <- c(pvalue_formats, if (nzchar(width)) paste0("%", width, "s") else "%s")
          }
        }
        previous_end <- matches[i] + lengths[i]
      }
      for (i in rev(seq_along(pvalue_arguments))) {
        argument_index <- pvalue_arguments[i]
        conversion_index <- matches[argument_index]
        conversion_length <- lengths[argument_index]
        args[[argument_index]] <- format.pval(args[[argument_index]], digits = 6, eps = .Machine$double.xmin)
        fmt <- paste0(substr(fmt, 1L, conversion_index - 1L), pvalue_formats[i], substr(fmt, conversion_index + conversion_length, nchar(fmt)))
      }
    }
  }
  do.call(base::sprintf, c(list(fmt), args))
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
    output <- utils::capture.output(
      switch(
        class(print_structure),
        "function"  = cat(print_structure()),
        "character" = cat(print_structure)
      )
    )
    writeLines(output)
  }
  invisible(x)
}
