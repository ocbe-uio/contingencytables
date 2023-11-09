#' @title Exact conditional and mid-P tests for the rxc table
#' @description Exact conditional and mid-P tests for the rxc table:
#' the Fisher-Freeman-Halton, Pearson, likelihood ratio, Kruskal-Wallis,
#' linear-by-linear, and Jonckheere-Terpstra tests.
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed counts (an rxc matrix)
#' @examples
#' Exact_cond_midP_tests_rxc(table_7.3)  # a 3x2 table
#' \dontrun{
#'   Exact_cond_midP_tests_rxc(table_7.6) # a 3x3 table
#' }
#' @export
#' @note Works only for 3x2 and 3x3 tables
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Exact_cond_midP_tests_rxc <- function(n) {
  validateArguments(mget(ls()))

  r <- nrow(n)
  c <- ncol(n)
  nip <- apply(n, 1, sum)
  npj <- apply(n, 2, sum)
  N <- sum(n)

  # Find all possible tables conditional on the row and column sums
  if (r == 3 && c == 2) {
    tables <- find_possible_tables_3x2(nip, npj)
  } else if (r == 3 && c == 3) {
    tables <- find_possible_tables_3x3(nip, npj)
  } else {
    stop("Too elaborate...")
  }

  # Calculate the observed test statistics
  # [Tobs_FFH, Tobs_Pearson, Tobs_LR, Tobs_KW, Tobs_lbl, Tobs_JT] = test_statistics(n, N, r, c, nip, npj);
  ans <- test_statisticsEcmt(n, N, r, c, nip, npj)
  Tobs_FFH <- ans[[1]]
  Tobs_Pearson <- ans[[2]]
  Tobs_LR <- ans[[3]]
  Tobs_KW <- ans[[4]]
  Tobs_lbl <- ans[[5]]
  Tobs_JT <- ans[[6]]

  # Calculate exact P- and mid-P values
  P_FFH <- 0
  P_Pearson <- 0
  P_LR <- 0
  P_KW <- 0
  P_lbl <- 0
  P_JT <- 0
  midP_FFH <- 0
  midP_Pearson <- 0
  midP_LR <- 0
  midP_KW <- 0
  midP_lbl <- 0
  midP_JT <- 0

  for (i in seq_len(nrow(tables))) {
    x <- rbind(tables[i, 1:c], tables[i, (c + 1):(2 * c)], tables[i, (2 * c + 1):(3 * c)])
    ans <- test_statisticsEcmt(x, N, r, c, nip, npj)
    f <- ans[[1]]
    T_Pearson <- ans[[2]]
    T_LR <- ans[[3]]
    T_KW <- ans[[4]]
    T_lbl <- ans[[5]]
    T_JT <- ans[[6]]

    P_FFH <- P_FFH + ifelse(f <= Tobs_FFH, 1, 0) * f
    midP_FFH <- midP_FFH + ifelse(f < Tobs_FFH, 1, 0) * f + 0.5 * ifelse(f == Tobs_FFH, 1, 0) * f

    P_Pearson <- P_Pearson + ifelse(T_Pearson >= Tobs_Pearson, 1, 0) * f
    midP_Pearson <- midP_Pearson + ifelse(T_Pearson > Tobs_Pearson, 1, 0) * f + 0.5 * ifelse(T_Pearson == Tobs_Pearson, 1, 0) * f

    P_LR <- P_LR + ifelse(T_LR >= Tobs_LR, 1, 0) * f
    midP_LR <- midP_LR + ifelse(T_LR > Tobs_LR, 1, 0) * f + 0.5 * ifelse(T_LR == Tobs_LR, 1, 0) * f

    P_KW <- P_KW + ifelse(T_KW >= Tobs_KW, 1, 0) * f
    midP_KW <- midP_KW + ifelse(T_KW > Tobs_KW, 1, 0) * f + 0.5 * ifelse(T_KW == Tobs_KW, 1, 0) * f

    P_lbl <- P_lbl + ifelse(T_lbl >= Tobs_lbl, 1, 0) * f
    midP_lbl <- midP_lbl + ifelse(T_lbl > Tobs_lbl, 1, 0) * f + 0.5 * ifelse(T_lbl == Tobs_lbl, 1, 0) * f

    P_JT <- P_JT + ifelse(T_JT >= Tobs_JT, 1, 0) * f
    midP_JT <- midP_JT + ifelse(T_JT > Tobs_JT, 1, 0) * f + 0.5 * ifelse(T_JT == Tobs_JT, 1, 0) * f
  }

  # Output arguments (P-values and mid-P values)
  results <- list()
  results$P_FFH <- P_FFH
  results$midP_FFH <- midP_FFH
  results$P_Pearson <- P_Pearson
  results$midP_Pearson <- midP_Pearson
  results$P_LR <- P_LR
  results$midP_LR <- midP_LR
  results$P_KW <- P_KW
  results$midP_KW <- midP_KW
  results$P_lbl <- P_lbl
  results$midP_lbl <- midP_lbl
  results$P_JT <- P_JT
  results$midP_JT <- midP_JT

  print_fun <- function() {
    cat_sprintf("\nExact Fisher-Freeman-Halton: P = %9.7f\n", P_FFH)
    cat_sprintf("Mid-P Fisher-Freeman-Halton: P = %9.7f\n", midP_FFH)
    cat_sprintf("Exact Pearson statistic:     P = %9.7f\n", P_Pearson)
    cat_sprintf("Mid-P Pearson statistic:     P = %9.7f\n", midP_Pearson)
    cat_sprintf("Exact LR statistic:          P = %9.7f\n", P_LR)
    cat_sprintf("Mid-P LR statistic:          P = %9.7f\n", midP_LR)
    cat_sprintf("Exact Kruskal-Wallis:        P = %9.7f\n", P_KW)
    cat_sprintf("Mid-P Kruskal-Wallis:        P = %9.7f\n", midP_KW)
    cat_sprintf("Exact linear-by-linear:      P = %9.7f\n", P_lbl)
    cat_sprintf("Mid-P linear-by-linear:      P = %9.7f\n", midP_lbl)
    cat_sprintf("Exact Jonckheere-Terpstra:   P = %9.7f\n", P_JT)
    cat_sprintf("Mid-P Jonckheere-Terpstra:   P = %9.7f\n", midP_JT)
  }
  return(contingencytables_result(results, print_fun))
}

find_possible_tables_3x2 <- function(nip, npj) {
  tables <- matrix(0, 0, 6)
  for (x11 in max(c(0, npj[1] - (nip[2] + nip[3]))):min(c(nip[1], npj[1]))) {
    x12 <- nip[1] - x11
    for (x21 in max(c(0, npj[1] - (x11 + nip[3]))):min(c(nip[2], npj[1] - x11))) {
      x22 <- nip[2] - x21
      x31 <- npj[1] - x11 - x21
      x32 <- nip[3] - x31
      x <- c(x11, x12, x21, x22, x31, x32)
      tables <- rbind(tables, x)
    }
  }
  return(tables)
}

find_possible_tables_3x3 <- function(nip, npj) {
  tables <- matrix(0, 0, 9)
  for (x11 in max(c(0, nip[1] - npj[2] - npj[3], npj[1] - nip[2] - nip[3])):min(c(nip[1], npj[1]))) {
    for (x12 in max(c(0, nip[1] - x11 - npj[3], npj[2] - nip[2] - nip[3])):min(c(nip[1] - x11, npj[2]))) {
      x13 <- nip[1] - x11 - x12
      for (x21 in max(c(0, nip[2] - npj[2] - x12 - npj[3] - x13, npj[1] - x11 - nip[3])):min(c(nip[2], npj[1] - x11))) {
        for (x22 in max(c(0, nip[2] - x21 - (npj[3] - x13), npj[2] - x12 - nip[3])):min(c(nip[2] - x21, npj[2] - x12))) {
          x23 <- nip[2] - x21 - x22
          x31 <- npj[1] - x11 - x21
          x32 <- npj[2] - x12 - x22
          x33 <- npj[3] - x13 - x23
          x <- c(x11, x12, x13, x21, x22, x23, x31, x32, x33)
          tables <- rbind(tables, x)
        }
      }
    }
  }
  return(tables)
}

test_statisticsEcmt <- function(x, N, r, c, nip, npj) {
  T_FFH <- multiple_hypergeomtric_pdf(x, N, r, c, nip, npj)
  ans <- Pearson_LR_statistics(x, N, r, c, nip, npj)
  T_Pearson <- ans[[1]]
  T_LR <- ans[[2]]
  T_KW <- KruskalWallis_statistic(x, N, r, c, nip, npj)
  T_lbl <- linear_by_linear_statistic(x, N, r, c)
  T_JT <- Jonckheere_Terpstra_statistic(x, r, c, nip)
  return(list(T_FFH, T_Pearson, T_LR, T_KW, T_lbl, T_JT))
}

Pearson_LR_statistics <- function(x, N, r, c, nip, npj) {
  m <- matrix(0, r, c)
  T_Pearson <- 0
  T_LR <- 0
  for (i in 1:r) {
    for (j in 1:c) {
      m[i, j] <- nip[i] * npj[j] / N
      if (m[i, j] > 0) {
        T_Pearson <- T_Pearson + ((x[i, j] - m[i, j])^2) / m[i, j]
      }
      if (x[i, j] > 0) {
        T_LR <- T_LR + x[i, j] * log(x[i, j] / m[i, j])
      }
    }
  }
  T_LR <- 2 * T_LR
  return(list(T_Pearson, T_LR))
}

KruskalWallis_statistic <- function(x, N, r, c, nip, npj) {
  midranks <- rep(0, c)
  for (j in 1:c) {
    midranks[j] <- ifelse(j > 1, sum(npj[1:(j - 1)]), 0) + (1 + npj[j]) / 2
  }
  R <- x %*% midranks
  CorrectionTerm <- 1
  for (j in 1:c) {
    CorrectionTerm <- CorrectionTerm - (npj[j]^3 - npj[j]) / (N^3 - N)
  }
  T0 <- 0
  for (i in 1:r) {
    T0 <- T0 + nip[i] * (R[i] / nip[i] - (N + 1) / 2)^2
  }
  T0 <- T0 * 12 / (N * (N + 1) * CorrectionTerm)
  return(T0)
}

#' @importFrom stats cor
linear_by_linear_statistic <- function(x, N, r, c) {
  # Use equally-spaced scores for both rows and columns
  a <- 1:r
  b <- 1:c
  Y1 <- rep(0, N)
  Y2 <- rep(0, N)
  id <- 0
  for (i in 1:r) {
    for (j in 1:c) {
      for (k in 1:x[i, j]) {
        id <- id + 1
        Y1[id] <- a[i]
        Y2[id] <- b[j]
      }
    }
  }
  r <- cor(Y1, Y2)
  T0 <- sqrt(N - 1) * r
  return(T0)
}

Jonckheere_Terpstra_statistic <- function(x, r, c, nip) {
  # Calculate the Mann-Whitney U statistic for the comparison of all pairs of
  # rows (i1, i2), for which i1 < i2
  U <- rep(0, r * (r - 1) / 2)
  id <- 0
  if (r > 1) {
    for (i1 in 1:(r - 1)) {
      for (i2 in (i1 + 1):r) {
        id <- id + 1

        # Work on the 2xc table with rows defined by i1 and i2
        npj_2xc <- apply(rbind(x[i1, ], x[i2, ]), 2, sum)

        # Calculate the midranks
        midranks <- rep(0, c)
        for (j in 1:c) {
          midranks[j] <- 0.5 * (ifelse(j > 1, sum(npj_2xc[1:(j - 1)]), 0) + 1 + sum(npj_2xc[1:j]))
        }
        W <- sum(x[i2, ] * midranks) # The Wilcoxon form of the WMW statistic
        U[id] <- W - nip[i2] * (nip[i2] + 1) / 2 # The Mann-Whitney form of the WMW statistic
      }
    }
  }

  # The Jonckheere-Terpstra test statistic
  T0 <- sum(U)

  return(T0)
}
