#' @rdname ensures_that
#' @export
check_that <- function(., ...)
{
  env  <- new.env(parent = parent.frame())
  env[["ensures_that"]] <- ensures_that

  args <- c(eval(substitute(alist(...))), fail_with = identity)
  cl <- as.call(c(quote(ensures_that), args))

  check_ <- eval(cl, env, env)

  identical(FALSE, inherits(check_(.), c("error", "simpleError")))
}

#' @rdname ensures_that
#' @export
check <- check_that
