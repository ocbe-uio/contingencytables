fill_nchoosek <- function(c0, npj) {
  npj_choose_x1j <- matrix(0, c0, max(npj) + 1)
    for (j in 1:c0) {
      for (x1j in 0:npj[j]) {
        npj_choose_x1j[j, x1j + 1] <- choose(npj[j], x1j)
      }
    }
  return(npj_choose_x1j)
}
