context("Seeking help")

test_that("list_functions() works as expected", {
  expect_error(list_functions(), "Please choose a chapter between 2 and 10")
  expect_error(list_functions(1), "Please choose a chapter between 2 and 10")
  for (chap in 2:10) {
    expect_message(
      expect_output(list_functions(chap)),
      paste("Chapter", chap, "functions:")
    )
  }
})
