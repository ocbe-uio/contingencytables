to_long <- function(n, a = NULL, b = NULL) {
  # If no scores are given, use equally spaced scores
  r <- nrow(n)
  c <- ncol(n)

  N <- sum(n)

  # Put the observed data into long format
  Y1 <- rep(0, N)
  Y2 <- rep(0, N)
  id <- 0
  for (i in seq_len(r)) {
    for (j in seq_len(c)) {
      for (k in seq_len(n[i, j])) {
        id <- id + 1
        if (is.null(a)) {
          Y1[id] <- i
        } else {
          Y1[id] <- a[i]
        }
        if (is.null(b)) {
          Y2[id] <- j
        } else {
          Y2[id] <- b[j]
        }
      }
    }
  }

  # The CI bootstrap sample
  data.frame(Y1 = Y1, Y2 = Y2)
}
