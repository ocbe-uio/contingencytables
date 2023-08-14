multiple_hypergeomtric_pdf <- function(x, N, r, c, nip, npj) {
  # edited version of this function to use logarithms of
  # factorials for intermediate computations to be able
  # to handle larger sample counts
  # --Brad J. Biggerstaff, US CDC (bkb5@cdc.gov)
  #   10 Aug 2023
  #

  cutoff <- floor(N / 2)
  Nfact1 <- lfactorial(cutoff)
  Nfact2 <- lfactorial(N) - lfactorial(cutoff)
  terms1 <- lfactorial(npj)
  terms2 <- lfactorial(nip)

  f <- sum(terms1) - Nfact1 + sum(terms2) - Nfact2 - sum(lfactorial(x))

  return(exp(f))
}
