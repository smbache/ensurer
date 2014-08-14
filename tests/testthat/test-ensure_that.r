context("Inline contracts using 'ensure_that'")

test_that("ensure_that works", {
	
	iris2 <-
		ensure_that(iris, is.data.frame(.))
	
  expect_that(iris, is_identical_to(iris2))
	
	iris_template <- iris[0, ]
	
	iris2 <-
		ensure_that(iris, identical(class(.), class(iris_template)))

	expect_that(iris, is_identical_to(iris))
	
	ensure_data_validity <- function(x, data_template)
	{
		data_template <- data_template
		ensure_that(x,
								identical(class(.), class(tpl)), 
								identical(names(.), names(tpl)),
								identical(sapply(., class), sapply(tpl, class)),
								tpl = data_template)
	}
	
	iris2 <- 
		ensure_data_validity(iris, iris_template)
	
	
	expect_that(iris, is_identical_to(iris2))
	
	expect_error(
		ensure_that(diag(3), NCOL(.) == 4, NROW(.) == 3),
		"NCOL(.)"
		)
	
})
