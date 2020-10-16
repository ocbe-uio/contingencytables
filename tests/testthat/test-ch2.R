context("Chapter 2")

test_that("Chapter 2 functions basically work", {
	expect_output(
		object = ch2$Wald_CI_1x2(100),
		expected = "The Wald CI: estimate = 0.4690 (95% CI 0.4267 to 0.5114)"
	)
	expect_output(
		object = ch2$Wald_CI_1x2(1, 2),
		expected = "The Wald CI: estimate = 0.5000 (95% CI 0.0000 to 1.0000)"
	)
	expect_output(
		object = ch2$AgrestiCoull_CI_1x2(19),
		expected = "The Agresti-Coull CI: estimate = 0.4693 (95% CI 0.4271 to 0.5115)"
	)
	expect_output(
		object = ch2$AgrestiCoull_CI_1x2(19, 20, .15),
		expected = "The Agresti-Coull CI: estimate = 0.8750 (85% CI 0.7778 to 0.9722)"
	)
})