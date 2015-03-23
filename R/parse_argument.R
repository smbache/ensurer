#' Parse a \code{function_} argument definition.
#'
#' An argument definition as passed to \code{function_} is checked and
#' parsed. The argument is expected to have the syntax \code{name ~ type: default}
#' where the \code{: default} is optional specification of the default value.
#'
#' Note that since a default value can be \code{NULL}, it is necessary to
#' check whether the default was specified in the argument, see the return value.
#'
#' @param arg A language object representing the argument.
#' @return list with the following entries:
#' \itemize{
#'   \item \code{type} character: the type
#'   \item \code{name} character: the name
#'   \item \code{has_default} logical: whether a default value was specified
#'   \item \code{default} the default value (or NULL if not specified.)
#' }
parse_argument <- function(arg)
{
  tilde <- quote(`~`)
  colon <- quote(`:`)

  if (is.symbol(arg))
    arg <- call("~", arg, quote(some))

  is_call    <- is.call(arg)
  is_formula <- is_call && identical(arg[[1L]], tilde)

  if (!is_formula)
    stop("Invalid argument specification.", call. = FALSE)

  has_default <- is.call(arg[[3L]]) && identical(arg[[3L]][[1L]], colon)

  if (!is.symbol(arg[[3L]]) && !(has_default && is.symbol(arg[[3L]][[2L]])))
    stop("Invalid type specification.", call. = FALSE)

  type <- sprintf("type_%s",
                  deparse(if (has_default) arg[[3L]][[2L]] else arg[[3L]],
                          nlines = 1L))

  name <- deparse(arg[[2L]], nlines = 1L)

  list(name = name,
       type = type,
       has_default = has_default,
       default = if (has_default) arg[[3L]][[3L]])
}
