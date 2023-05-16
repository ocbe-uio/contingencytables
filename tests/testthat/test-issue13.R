context("Generation of table combinations")

# Original functions (for benchmarking) ========================================
# Also good to keep these in case they need to be reimplemented
# (in another language)
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
  return(x)
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
  return(x)
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
  return(x)
}

# Unit tests ===================================================================
test_that("Results for the new functions are the same", {
  for (iter in seq_len(5)) {
    n_large <- sample(50:80, 1L)
    n_small <- sample(20:50, 1L)
    n_bitty <- sample(10:20, 1L)
    n_micro <- sample(5:10, 1L)
    expect_equal(all_tables_X(n_large, 3), at3(n_large))
    expect_equal(all_tables_X(n_small, 4), at4(n_small))
    expect_equal(all_tables_X(n_bitty, 5), at5(n_bitty))
    expect_equal(all_tables_X(n_micro, 6), at6(n_micro))
    expect_equal(all_tables_X(n_micro, 7), at7(n_micro))
  }
})
