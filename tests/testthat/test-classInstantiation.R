context("Class instantiation")

single_name <- "contingencytables_singletest"
multiple_name <- "contingencytables_multipletests"
single_content <- list(name = "test", statistics = pi)
multiple_content <- list(statistics = pi, FUN = function(statistics) "hi")

test_that("Invalid classes are rejected", {
  expect_error(
    newContingencytablesOutput(single_content, "this is fine"),
    "Invalid class_name"
  )
  expect_error(
    newContingencytablesOutput(multiple_content, "numeric"),
    "Invalid class_name"
  )
})

test_that("Invalid content is rejected", {
  expect_error(
    newContingencytablesOutput(pi, single_name),
    "Invalid content\\. .+ name, statistics"
  )
  expect_error(
    newContingencytablesOutput(pi, multiple_name),
    "Invalid content\\. .+ statistics, FUN"
  )
  expect_error(
    newContingencytablesOutput(list(statistics = 1, FUN = sd), multiple_name),
    "FUN_arguments == \"statistics\" are not all TRUE"
  )
})

test_that("Invalid combinations are rejected", {
  expect_error(
    newContingencytablesOutput(single_content, multiple_name),
    "Invalid content\\. .+ statistics, FUN"
  )
  expect_error(
    newContingencytablesOutput(multiple_content, single_name),
    "Invalid content\\. .+ name, statistics"
  )
})

test_that("Valid content is returned and printed correctly", {
  s <- newContingencytablesOutput(single_content, single_name)
  m <- newContingencytablesOutput(multiple_content, multiple_name)
  expect_s3_class(s, single_name)
  expect_s3_class(m, multiple_name)
  expect_equal(s, structure(single_content, class = single_name))
  expect_equal(m, structure(multiple_content, class = multiple_name))
  expect_output(print(s))
  expect_output(print(m))
})
