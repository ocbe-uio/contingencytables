#' @export
plot.contingencytables_result <- function(x, y, ...) {
  P <- max(x$p_values)
  index <- which(P == x$p_values)
  common_pi_at_max_value <- x$pi_values[index]
  plot(x$pi_values, x$p_values, lty = 1, col = "black", ...)
  lines(
    c(common_pi_at_max_value, common_pi_at_max_value), c(0, P),
    lty = 2, col = "red"
  )
  invisible()
}
