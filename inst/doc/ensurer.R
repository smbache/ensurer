## ----, eval=FALSE--------------------------------------------------------
#  1. ensure_that:  function(value., ..., fail_with, err_desc)  [short-hand alias: ensure]
#  2. ensures_that: function(..., fail_with, err_desc)          [short-hand alias: ensures]

## ----, eval=FALSE--------------------------------------------------------
#  the_matrix <-
#    get_matrix() %>%
#    ensure_that(is.numeric(.),
#                NCOL(.) == NROW(.))

## ----eval=FALSE----------------------------------------------------------
#   Error: conditions failed for call 'get_matrix %>% ensu .. NCOL(.) == NROW(.))':
#     * is.numeric(.)

## ----eval=FALSE----------------------------------------------------------
#  ensure_square_numeric <-
#    ensures_that(NCOL(.) == NROW(.),
#                 is.numeric(.))
#  
#  m1 <- get_matrix()       %>% ensure_square_numeric
#  m2 <- get_other_matrix() %>% ensure_square_numeric

## ----eval=FALSE----------------------------------------------------------
#  m3 <-
#    get_matrix() %>%
#    ensure_square_numeric %>%
#    ensure_that(all(. < 10))

## ----eval=FALSE----------------------------------------------------------
#  # Only the first error is recorded.
#  letters %>%
#    ensure_that(length(.) == 10) %>%
#    ensure_that(all(. == toupper(.)))
#  
#  # Both errors are recorded.
#  letters %>%
#    ensure_that(length(.) == 10,
#                all(. == toupper(.)))

## ----eval=FALSE----------------------------------------------------------
#  the_matrix <-
#    ensure_that(get_matrix(),
#        NCOL(.) == NROW(.),
#        is.numeric(.))

## ----, eval = FALSE------------------------------------------------------
#  some_object <-
#    some_computation() %>%
#    ensure_that(foo(a) == bar(a), a = baz(x, y, z))

## ----, eval = FALSE------------------------------------------------------
#  matrix_is_square <- ensures_that(NROW(.) == NCOL(.))
#  all_positive     <- ensures_that(all(. > 0))
#  
#  
#  matrix(runif(16), 4, 4) %>%
#    ensure(+matrix_is_square, +all_positive)

## ----, eval = FALSE------------------------------------------------------
#  ensure_character <-
#    ensures_that(is.character(.) ~ "vector must be of character type.")
#  
#  1:10 %>% ensure_character
#  
#  Error: conditions failed for call '1:10 %>% ensure_character':
#     * vector must be of character type.

## ----eval=FALSE----------------------------------------------------------
#  `: numeric` <- ensures_that(is.numeric(.))
#  
#  get_quote <- function(ticker) `: numeric`({
#    # some code that extracts latest quote for the ticker
#  })
#  
#  
#  `: square matrix` <- ensures_that(is.numeric(.),
#                                    is.matrix(.),
#                                    NCOL(.) == NROW(.))
#  
#  var_covar <- function(vec) `: square matrix`({
#    # some code that produces a variance-covariance matrix.
#  })

## ----eval=FALSE----------------------------------------------------------
#  square_failure <- function(e)
#  {
#  	# suppose you had an email function:
#  	email("maintainer@company.com", subject = "error", body = e$message)
#  	stop(e)
#  }
#  
#  m1 <-
#    get_matrix() %>%
#    ensure_that(
#        NCOL(.) == NROW(.),
#        is.numeric(.),
#        fail_with = square_failure)
#  
#  m2 <-
#    get_matrix() %>%
#    ensure_that(
#        NCOL(.) == NROW(.),
#        is.numeric(.),
#        fail_with = diag(10))

## ----eval=FALSE----------------------------------------------------------
#  square_failure <- function(e)
#  {
#    # fetch the dot.
#    . <- get(".", parent.frame())
#  
#    # compose a message detailing also the class of the object returned.
#    msg <-
#      sprintf("Here is what I know:\n%s\nValue class: %s.",
#              e$message,
#              class(.) %>% paste(collapse = ", ")) # there could be several.
#  
#    # suppose you had an email function:
#    email("maintainer@company.com", subject = "error", body = msg)
#  
#    stop(e)
#  }

## ----eval=FALSE----------------------------------------------------------
#  `: sql result` <-
#      ensures_that(is.data.frame(.),
#                   err_desc = "SQL error.")
#  
#  daily_results <- function(day) `: sql result`({
#      sql <- sprintf("SELECT * FROM RESULTS WHERE DAY = '%s'",
#                     format(as.Date(day)))
#      ch <- RODBC::odbcDriverConnect("connection_string")
#      on.exit(RODBC::odbcClose(ch))
#      RODBC::sqlQuery(ch, sql)
#  })

## ----, eval=FALSE--------------------------------------------------------
#  daily_results("2014-10-01")
#  
#   Error: conditions failed for call 'daily_results("2014-10-01")':
#     * is.data.frame(.)
#   Description: SQL error.

## ----eval=FALSE----------------------------------------------------------
#  iris_template <-
#    data.frame(
#      Sepal.Length = numeric(0),
#      Sepal.Width  = numeric(0),
#      Petal.Length = numeric(0),
#      Petal.Width  = numeric(0),
#      Species      =
#          factor(numeric(0), levels = c("setosa", "versicolor", "virginica"))
#  )
#  
#  ensure_as_template <- function(x, tpl)
#    ensure_that(x,
#      is.data.frame(.),
#      identical(class(.), class(tpl)),
#      identical(sapply(., class), sapply(tpl, class)),
#      identical(sapply(., levels), sapply(tpl, levels))
#    )
#  
#  iris %>% ensure_as_template(iris_template)

