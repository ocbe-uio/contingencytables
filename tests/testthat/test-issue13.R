context("Generation of table combinations")

# Original functions (for benchmarking) ========================================
at3 <- function(N) {
  x <- vector()
  for (x1 in 0:N) {
    for (x2 in 0:(N - x1)) {
      x <- rbind(x, c(x1, x2, N - x1 - x2))
    }
  }
  return(x)
}
at4 <- function(N) {
  x <- vector()
  for (x1 in (0:N)) {
    for (x2 in 0:(N - x1)) {
      for (x3 in 0:(N - x1 - x2)) {
        x <- rbind(x, c(x1, x2, x3, N - x1 - x2 - x3))
      }
    }
  }
  return(x)
}
at5 <- function(N) {
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
}
at6 <- function(N) {
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
}
at7 <- function(N) {
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
}

# Unit test ====================================================================
test_that("Results for the new functions are the same", {
  for (i in seq_len(50)) {
    expect_equal(all_tables_3(i), at3(i))
  }
})
