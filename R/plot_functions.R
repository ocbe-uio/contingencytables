#' @export
plot.contingencytables_result <- function(x, y, ...) {
  # Validating  ================================================================
  if (!identical(names(x), c("Pvalue", "Pvalues", "pi_values"))) {
    stop(
      "Plotting is only supported for the following functions:\n",
      "    - Exact_unconditional_test_2x2()\n",
      "    - McNemar_exact_unconditional_test_paired_2x2()"
    )
  }

  # Calculating plotting elements ==============================================
  P <- max(x$Pvalues)
  index <- which(P == x$Pvalues)
  common_pi_at_max_value <- x$pi_values[index]

  # Plotting ===================================================================
  plot(x$pi_values, x$Pvalues, lwd = 2, type = "l", ...)
  lines(
    c(common_pi_at_max_value, common_pi_at_max_value), c(0, P),
    lty = 2, col = "red"
  )
  invisible()
}
