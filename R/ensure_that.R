#' Ensure certain conditions for a value at runtime.
#'
#' @param value. The value which is to be ensured.
#' @param ... Conditions which must pass for the ensurance to be fulfilled.
#' Any named argument will treated as values available when evaluating the 
#' conditions. To reference the value itself use the dot-placeholder, `.`
#' @return value.
#' @details if value. does not comply with conditions an error is raised.
#' @rdname ensure_that
#' @examples
#' \dontrun{
#' library(magrittr)
#'
#' # Will pass
#' matrix(runif(16), 4, 4) %>%
#' 	 ensure_that(ncol(.) == nrow(.), all(. <= 1))
#' 	 
#' # Will raise an error
#' matrix(NA, 4, 4) %>% 
#'   ensure_that(. %>% anyNA %>% not)
#' }
#' @export
ensure_that <- function(value., ...)
{
	parent <- parent.frame()
	dots   <- eval(substitute(alist(...)))
	e      <- eval(as.call(c(quote(ensures_that), dots)), parent, parent)
	
	e(value.)
}