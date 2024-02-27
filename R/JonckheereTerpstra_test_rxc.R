#' @title The Jonckheere-Terpstra test for association
#' @description The Jonckheere-Terpstra test for association
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed table (an rxc matrix)
#' @examples
#' JonckheereTerpstra_test_rxc(table_7.7)
#' JonckheereTerpstra_test_rxc(table_7.8)
#' JonckheereTerpstra_test_rxc(table_7.9)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
JonckheereTerpstra_test_rxc <- function(n) {
  validateArguments(mget(ls()))

  r <- nrow(n)
  c <- ncol(n)

  nip <- apply(n, 1, sum)
  npj <- apply(n, 2, sum)
  N <- sum(n)

  # Calculate the Mann-Whitney U statistic for the comparison of all pairs of
  # rows (i1, i2), for which i1 < i2
  U <- rep(0, r * (r - 1) / 2)
  id <- 0
  for (i1 in 1:(r - 1)) {
    for (i2 in (i1 + 1):r) {
      id <- id + 1

      # Work on the 2xc table with rows defined by i1 and i2
      npj_2xc <- apply(rbind(n[i1, ], n[i2, ]), 2, sum)

      # Calculate the midranks
      midranks <- rep(0, c)
      for (j in 1:c) {
        if (j > 1) {
          midranks[j] <- 0.5 * (sum(npj_2xc[1:(j - 1)]) + 1 + sum(npj_2xc[1:j]))
        } else {
          midranks[j] <- 0.5 * (1 + sum(npj_2xc[1:j]))
        }
      }
      W <- sum(n[i2, ] * midranks) # The Wilcoxon form of the WMW statistic
      U[id] <- W - nip[i2] * (nip[i2] + 1) / 2 # The Mann-Whitney form of the WMW statistic
    }
  }

  # The Jonckheere-Terpstra test statistic
  T0 <- sum(U)

  # Expectation of T
  ExpT <- (N^2 - sum(nip^2)) / 4

  # Variance of T
  A <- sum(nip * (nip - 1) * (2 * nip + 5))
  B <- sum(npj * (npj - 1) * (2 * npj + 5))
  C <- sum(nip * (nip - 1) * (nip - 2))
  D <- sum(npj * (npj - 1) * (npj - 2))
  E <- sum(nip * (nip - 1))
  F <- sum(npj * (npj - 1))
  VarT <- (N * (N - 1) * (2 * N + 5) - A - B) / 72
  VarT <- VarT + C * D / (36 * N * (N - 1) * (N - 2))
  VarT <- VarT + E * F / (8 * N * (N - 1))

  # The standard normalized Jonckheere-Terpstra test statistic
  Z <- (T0 - ExpT) / sqrt(VarT)
  P <- 2 * (1 - pnorm(abs(Z), 0, 1))

  return(
    contingencytables_result(
      list("Pvalue" = P, "Z" = Z),
      sprintf("The Jonckheere-Terpstra test for association: P = %8.6f, Z = %6.3f", P, Z)
    )
  )
}
