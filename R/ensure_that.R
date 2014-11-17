#' @rdname ensures_that
#' @export
ensure_that <- function(value., ..., fail_with = function(e) stop(e), err_desc = "")
{
  parent <- parent.frame()
  dots   <- eval(substitute(alist(...)))
  e      <- eval(as.call(c(quote(ensures_that), dots,
                           fail_with = fail_with,
                           err_desc = err_desc)),
                 parent, parent)

  e(value.)
}

#' @rdname ensures_that
#' @export
ensure <- ensure_that
