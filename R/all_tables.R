# Maybe there's a way to use recursion instead of all these level_*_size()
# functions. I couldn't figure it out, but feel free to give it a try!
level_3_size <- function(N) {
  sum(1:(N + 1))
}

level_4_size <- function(N) {
  size <- 0L
  for (iter in 0:N) {
    size <- size + level_3_size(iter)
  }
  return(size)
}

level_5_size <- function(N) {
  size <- 0L
  for (iter in 0:N) {
    size <- size + level_4_size(iter)
  }
  return(size)
}

level_6_size <- function(N) {
  size <- 0L
  for (iter in 0:N) {
    size <- size + level_5_size(iter)
  }
  return(size)
}

level_7_size <- function(N) {
  size <- 0L
  for (iter in 0:N) {
    size <- size + level_6_size(iter)
  }
  return(size)
}

all_tables_3 <- function(N) {
  x0 <- unlist(sapply(0:N, function(x) rep(x, N - x + 1)))
  x1 <- unlist(sapply(N:0, function(x) seq(0, x, 1)))
  x2 <- unlist(sapply(N:0, function(x) seq(x, 0, -1)))
  return(cbind(x0, x1, x2, deparse.level = 0))
}

all_tables_4 <- function(N) {
  x <- vector()
  first_col <- 0L
  for (i in N:0) {
    other_cols <- all_tables_3(i)
    sub_x <- cbind(rep(first_col, nrow(other_cols)), other_cols)
    x <- rbind(x, sub_x)
    first_col <- first_col + 1L
  }
  return(x)
}

all_tables_5 <- function(N) {
  x <- vector()
  for (x1 in 0:N) {
    for (x2 in 0:(N - x1)) {
      for (x3 in 0:(N - x1 - x2)) {
        for (x4 in 0:(N - x1 - x2 - x3)) {
          x <- rbind(x, c(x1, x2, x3, x4, N - x1 - x2 - x3 - x4))
        }
      }
    }
  }
  return(x)
}

all_tables_6 <- function(N) {
  x <- vector()
  for (x1 in 0:N) {
    for (x2 in 0:(N - x1)) {
      for (x3 in 0:(N - x1 - x2)) {
        for (x4 in 0:(N - x1 - x2 - x3)) {
          for (x5 in 0:(N - x1 - x2 - x3 - x4)) {
            x <- rbind(x, c(x1, x2, x3, x4, x5, N - x1 - x2 - x3 - x4 - x5))
          }
        }
      }
    }
  }
  return(x)
}

all_tables_7 <- function(N) {
  x <- vector()
  for (x1 in 0:N) {
    for (x2 in 0:(N - x1)) {
      for (x3 in 0:(N - x1 - x2)) {
        for (x4 in 0:(N - x1 - x2 - x3)) {
          for (x5 in 0:(N - x1 - x2 - x3 - x4)) {
            for (x6 in 0:(N - x1 - x2 - x3 - x4 - x5)) {
              x <- rbind(
                x,
                c(x1, x2, x3, x4, x5, x6, N - x1 - x2 - x3 - x4 - x5 - x6)
              )
            }
          }
        }
      }
    }
  }
  return(x)
}
