#' Define a type-safe function.
#'
#' Using \code{function_} is an alternative way to define a function where
#' arguments are type-safe. Arguments are specified as \code{name ~ type} or as
#' \code{type:name ~ value}. The last argument to \code{function_} is the body
#' of the function. If no type is specified, it will default to
#' \code{some}, which is true for anything.
#'
#' @param ... Arguments for the function and the function body as the last
#'   argument.
#'
#' @export
#' @return a type-safe function
#' @examples
#' f <- function_(a ~ integer, b ~ character: "Hello, World", {
#'   rep(b, a)
#' })
function_ <- function(...)
{
  # extract un-evaluated dot arguments.
  dots <- eval(substitute(alist(...)))

  # Check that at least a body is provided.
  if (length(dots) == 0L)
    stop("At least the body of the function must be returned.", call. = FALSE)

  # Split into arguments and body.
  args <- if (length(dots) > 1) lapply(dots[-length(dots)], parse_argument)
  body <- dots[[length(dots)]]

  # Construct a formal argument list.
  arg_list <- lapply(1:length(args), function(.) alist(.=)[[1L]])
  names(arg_list) <- vapply(args, `[[`, character(1), i = "name")
  for(i in 1:length(arg_list))
    if (args[[i]][["has_default"]])
      arg_list[[i]] <- args[[i]][["default"]]

    # Construct the type check calls.
    contracts <-
      lapply(args, function(a) call(a[["type"]], as.name(a[["name"]])))

    # Join with existing body
    body <- as.call(c(list(quote(`{`)), contracts, body))

    # Make the function
    f <- eval(call("function", as.pairlist(arg_list), body))

    # Let types be overridden by user
    for (name in vapply(args, `[[`, character(1L), i = "type"))
      if (exists(name, parent.frame(), inherits = TRUE, mode = "function"))
        environment(f)[[name]] <- get(name, parent.frame(), inherits = TRUE)

    # return the resulting function
    f
}
