to_long <- function(n, a = NULL, b = NULL) {
  # If no scores are given, use equally spaced scores
  r <- nrow(n)
  c <- ncol(n)

  N <- sum(n)

  # Put the observed data into long format
  Y1 <- rep(0, N)
  Y2 <- rep(0, N)
  id <- 0
  for (i in 1:r) {
    for (j in 1:c) {
      for (k in 1:n[i, j]) {
        id <- id + 1
        Y1[id] <- a[i]
        Y2[id] <- b[j]
      }
    }
  }

  # The CI bootstrap sample
  data.frame(Y1 = Y1, Y2 = Y2)
}
