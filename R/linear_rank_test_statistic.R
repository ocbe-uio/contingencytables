linear_rank_test_statistic <- function(x, y) {
  # x: a vector of counts
  # y: a vector of scores
  # Returns the linear rank test statistic
  # The sum of the products of the counts and the scores
  sum(x * y)
}
