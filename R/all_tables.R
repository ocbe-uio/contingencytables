all_tables_3 <- function(N) {
  x0 <- unlist(lapply(0:N, function(x) rep(x, N - x + 1)))
  x1 <- unlist(lapply(N:0, function(x) seq(0, x, 1)))
  x2 <- unlist(lapply(N:0, function(x) seq(x, 0, -1)))
  return(cbind(x0, x1, x2, deparse.level = 0))
}

all_tables_X <- function(N, X) {
  if (X < 3) stop("X must be >= 3")
  if (X == 3) return(all_tables_3(N))
  x <- vector()
  first_col <- 0L
  for (i in N:0) {
    other_cols <- all_tables_X(i, X - 1L)
    sub_x <- cbind(rep(first_col, nrow(other_cols)), other_cols)
    x <- rbind(x, sub_x)
    first_col <- first_col + 1L
  }
  return(x)
}
