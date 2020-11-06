context("Chapter 2")

test_that("Chapter 2 functions basically work", {
	load_chapter(2)
	expect_output(
		object = Wald_CI_1x2(100),
		expected = "The Wald CI: estimate = 0.4690 (95% CI 0.4267 to 0.5114)"
	)
	expect_output(
		object = Wald_CI_1x2(1, 2),
		expected = "The Wald CI: estimate = 0.5000 (95% CI 0.0000 to 1.0000)"
	)
	expect_output(
		object = AgrestiCoull_CI_1x2(19),
		expected = "The Agresti-Coull CI: estimate = 0.4693 (95% CI 0.4271 to 0.5115)"
	)
	expect_output(
		object = AgrestiCoull_CI_1x2(19, 20, .15),
		expected = "The Agresti-Coull CI: estimate = 0.8750 (85% CI 0.7778 to 0.9722)"
	)
	expect_output(
		object = Arcsine_CI_1x2(500),
		expected = "The arcsine CI: estimate = 0.4690 (95% CI 0.4269 to 0.5115)"
	)

	expect_output(
		object = Arcsine_CI_1x2(100, 5e3, .1),
		expected = "The arcsine CI: estimate = 0.0200 (90% CI 0.0169 to 0.0235)"
	)
	expect_output(
		object = Blaker_exact_CI_1x2(1),
		expected = "The Blaker exact CI: estimate = 0.8125 (95% CI 0.5656 to 0.9469)"
	)
	expect_output(
		object = Blaker_exact_CI_1x2(1, 100),
		expected = "The Blaker exact CI: estimate = 0.0100 (95% CI 0.0005 to 0.0513)"
	)
	expect_output(
		object = Blaker_exact_test_1x2(1),
		expected = "The Blaker exact test: P = 0.04615"
	)
	expect_output(
		object = Blaker_exact_test_1x2(1, 10, .5),
		expected = "The Blaker exact test: P = 0.02148"
	)
})