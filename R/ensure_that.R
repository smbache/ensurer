#' @rdname ensures_that
#' @export
ensure_that <- function(value., ...)
{
  parent <- parent.frame()
  dots   <- eval(substitute(alist(...)))
  e      <- eval(as.call(c(quote(ensures_that), dots)), parent, parent)

  e(value.)
}
