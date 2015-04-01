#' @rdname ensures_that
#' @export
ensure_that <- function(., ..., fail_with = function(e) stop(e), err_desc = "")
{
  env  <- new.env(parent = parent.frame())
  env[["ensures_that"]] <- ensures_that

  args <- c(eval(substitute(alist(...))),
            fail_with = fail_with, err_desc  = err_desc)

  cl <- as.call(c(quote(ensures_that), args))

  ensure_  <- eval(cl, env, env)

  ensure_(.)
}

#' @rdname ensures_that
#' @export
ensure <- ensure_that

