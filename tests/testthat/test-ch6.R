context("Chapter 6")

test_that("Chapter 6 functions basically work", {
	load_chapter(6)
	n <- rbind(c(14, 10, 3, 2), c(11, 7, 8, 4))
	expect_output(
		object = Brant_test_2xc(n) ,
		regexp = "Brant test: T =  1.668, df = 2, P = 0.43422"
	)
	# expect_output(
	# 	object = ,
	# 	regexp = ""
	# )
	unload_chapter(6)
})
