all_tables_3 <- function(N) {
  x0 <- unlist(sapply(0:N, function(x) rep(x, N - x + 1)))
  x1 <- unlist(sapply(N:0, function(x) seq(0, x, 1)))
  x2 <- unlist(sapply(N:0, function(x) seq(x, 0, -1)))
  return(cbind(x0, x1, x2, deparse.level = 0))
}
