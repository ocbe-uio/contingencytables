context("Chapter 9")

test_that("Chapter 8 functions basically work", {
	load_chapter(9)
	n <- rbind(c(596, 18, 6, 5), c(0, 2, 0, 0), c(0, 0, 42, 0), c(11, 0, 0, 0))
	expect_output(
		object = Bhapkar_test_paired_cxc(n),
		regexp = "marginal homogenity: P = 0.000005, T = 27.304 \\(df=3\\)"
	)
	expect_output(
		object = Bonferroni_type_CIs_paired_cxc(n),
		regexp = "pi_4\\+ vs pi_ \\+ 4: delta =  0.0088 \\(-0.0059 to  0.0233\\)"
	)
	# expect_output(
	# 	object = (n),
	# 	regexp = ""
	# )
	# expect_output(
	# 	object = (n),
	# 	regexp = ""
	# )
	# expect_output(
	# 	object = (n),
	# 	regexp = ""
	# )
	# expect_output(
	# 	object = (n),
	# 	regexp = ""
	# )
	# expect_output(
	# 	object = (n),
	# 	regexp = ""
	# )
	# expect_output(
	# 	object = (n),
	# 	regexp = ""
	# )
	# expect_output(
	# 	object = (n),
	# 	regexp = ""
	# )
	# expect_output(
	# 	object = (n),
	# 	regexp = ""
	# )
	# expect_output(
	# 	object = (n),
	# 	regexp = ""
	# )
	# expect_output(
	# 	object = (n),
	# 	regexp = ""
	# )
	unload_chapter(8)
})
