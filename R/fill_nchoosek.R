fill_nchoosek <- function(c0, npj) {
  npj_choose_x1j <- lapply(
    npj,
    function(n) vapply(0:n, function(k) choose(n, k), 0)
  )
  max_length <- max(vapply(npj_choose_x1j, length, 0))
  npj_choose_x1j <- t(vapply(npj_choose_x1j, function(x) c(x, rep(0, max_length - length(x))), rep(0, max_length)))
  return(npj_choose_x1j)
}
