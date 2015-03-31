context("ensurer tests")

test_that("ensure(s)_that works", {

	iris2 <-
		ensure_that(iris, is.data.frame(.))

  expect_that(iris, is_identical_to(iris2))

	iris_template <- iris[0, ]

	iris2 <-
		ensure_that(iris, identical(class(.), class(iris_template)))

	expect_that(iris, is_identical_to(iris2))

	ensure_data_validity <- function(x, data_template)
	{
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

  expect_error(
    ensure_that(c(1, NA), !any(is.na(.)) ~ "Custom Error"),
    "Custom Error"
  )

  square_type <- ensures_that(NCOL(.) == NROW(.) ~ "Not square")
  A <- matrix(1:16, 4, 4)
  B <- ensure(A, all(. > 0), +square_type)

	expect_that(A, is_identical_to(B))

  expect_error(
    ensure_that(matrix(1:10, 5, 2), all(. > 0), +square_type),
    "Not square"
  )

  expect_identical(1:10, ensure(1:10, is.integer))

})
