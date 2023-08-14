multiple_hypergeomtric_pdf <- function(x, N, r, c, nip, npj) {
  if (any(max(x) > 170, nip > 170, npj > 170)) {
    return(NA)
  }
  # Somewhat messy code to avoid overflow
  if (N > 170) {
    cutoff <- 170
  } else {
    cutoff <- floor(N / 2)
  }
  Nfact1 <- factorial(cutoff)
  Nfact2 <- 1
  for (i in (cutoff + 1):N) {
    Nfact2 <- Nfact2 * i
  }
  terms1 <- factorial(npj)
  terms2 <- factorial(nip)
  f <- 1 / Nfact1
  f <- f * terms1[1]
  f <- f * prod(terms1[-1])
  f <- f / Nfact2
  f <- f * terms2[1]
  f <- f * prod(terms2[-1])
  for (i in 1:r) {
    for (j in 1:c) {
      f <- f / factorial(x[i, j])
    }
  }
  return(f)
}
