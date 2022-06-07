context("Cross-chapter integration")

test_that("Wilson_score_CI_1x2 works across chapters", {
  expect_output(
    object  = the_1x2_table_CIs(X = 100, n = 1e3),
    regexpr = "Wilson score with CC         0.082 to 0.121    0.038"
  )
  suppressOutput <- function(fun_name) {
    FUN <- get(fun_name)
    mx <- matrix(c(123, 456, 789, 0), nrow = 2)
    out <- invisible(capture.output(FUN(mx)))
    return(out)
  }
  expect_length(suppressOutput("Newcombe_hybrid_score_CI_2x2"), 1)
  expect_length(suppressOutput("MOVER_R_Wilson_CI_ratio_2x2"), 1)
  expect_length(suppressOutput("MOVER_R_Wilson_CI_OR_2x2"), 1)
  expect_length(suppressOutput("MOVER_Wilson_score_CI_paired_2x2"), 1)
  expect_length(suppressOutput("Newcombe_square_and_add_CI_paired_2x2"), 1)
  expect_length(suppressOutput("Transformed_Wilson_score_CI_paired_2x2"), 1)
})
