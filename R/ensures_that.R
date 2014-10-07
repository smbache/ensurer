#' Ensure certain conditions for a value at runtime.
#'
#' Use \code{ensure_that} (imperitive form) to ensure conditions for
#' a value "on the fly". The present tense form, \code{ensures_that}
#' is used to make reusable "contracts" (functions) which can subsequently be
#' applied to values, see examples.
#'
#' @details There are special named arguments which can be used to
#' tweak the behavior upon failure:
#' \tabular{ll}{
#' \code{fail_with} \tab a static value, or a unary function which accept a
#' \code{simpleError} as argument.\cr
#' \code{err_desc} \tab can be specified to append a description upon error.
#' This can be useful when the same conditions occur at different places.
#' }
#' It is also possible to specify custom error message to conditions
#' to make them more readable. To do this use a formula
#' \code{condition ~ message}, where \code{message} is a single character value.
#'
#' Finally, you can add an existing contract as a condition argument, which
#' will add the conditions from the existing contract to the new contract
#' (along with any assigned values). To do this use (unary) \code{+} to indicate
#' that an argument is a contract. See example below.
#' @param value. The value which is to be ensured.
#' @param ... conditions which must pass for the ensuring contract to be
#'        fulfilled. Any named argument will treated as values available
#'        when evaluating the conditions. To reference the value itself
#'        use the dot-placeholder, \code{`.`}. See 'Details' for some special
#'        named arguments.
#' @return \code{ensures_that} returns an ensuring function; \code{ensure_that}
#' returns the value itself on success.
#' @rdname ensures_that
#' @examples
#' \dontrun{
#' library(magrittr)
#'
#' # Create a contract which can ensure that a matrix is square.
#' ensure_square <- ensures_that(NCOL(.) == NROW(.))
#'
#' # apply it.
#' A <-
#'   diag(4) %>%
#'   ensure_square
#'
#' # Without the pipe operator:
#' A <- ensure_square(diag(4))
#'
#' # Ensure on the fly (this will pass the test)
#' A <-
#'   matrix(runif(16), 4, 4) %>%
#'   ensure_that(ncol(.) == nrow(.), all(. <= 1))
#'
#' # This will raise an error
#' A <-
#'   matrix(NA, 4, 4) %>%
#'   ensure_that(. %>% anyNA %>% not)
#'
#' # Tweak failure:
#' A <-
#'   1:10 %>%
#'   ensure_that(all(. < 5), err_desc = "Number tests!")
#'
#' # A default value for failure situations:
#' A <-
#'   1:10 %>%
#'   ensure_that(all(. < 5), fail_with = NA)
#'
#' # Suppose you had an email function:
#' email_err <- function(e) {email(e$message); stop(e)}
#'
#' A <-
#'   1:10 %>%
#'   ensure_that(all(. < 5), fail_with = email_err)
#'
#' # Two similar contracts, one extending the other.
#' # Note also that custom message is used for A
#' A <- ensures_that(all(.) > 0 ~ "Not all values are positive)
#' B <- ensures_that(!is.na(.) ~ "There are missing values", +A)
#'
#' B(c(-5:5, NA))
#' }
#' @export
ensures_that <- function(...)
{
  dots   <- eval(substitute(alist(...)))
  names  <- names(dots)
  named  <- if (is.null(names)) rep(FALSE, length(dots)) else names != ""

  if (sum(!named) == 0)
    stop("At least one condition is needed for an ensurance.", call. = FALSE)

  has_custom_msg <-
    which(vapply(dots,
                 function(cl) is.call(cl) && identical(cl[[1L]], quote(`~`)),
                 logical(1L)))

  env <- ensurer_env(parent = parent.frame())

  dots[has_custom_msg] <-
    lapply(has_custom_msg,
           function(i) `attr<-`(dots[[i]][[2L]], "custom_msg",
                                eval(dots[[i]][[3L]], env, env)))

  contracts <-
    vapply(dots,
          function(cl) is.call(cl) && identical(cl[[1L]], quote(`+`)),
          logical(1))

  if (sum(contracts) > 0)
    dots[contracts] <- lapply(dots[contracts],
                              function(cl) eval(cl[[2L]], env, env))

  contracts_conditions <-
    unlist(lapply(dots[contracts], function(contract) {
      environment(contract)[["__conditions"]]
    }))

  reserved <-
    c("__conditions", "__falsify", "__format_call", "__self",
      "__verify", "err_desc", "fail_with")

  for (contract in which(contracts)) {
    values <- ls(environment(dots[[contract]]))
    for (value in values[!values %in% c(reserved)])
      assign(value, environment(dots[[contract]])[[value]], env)
  }

  env[["__conditions"]] <- c(dots[!named & !contracts], contracts_conditions)

  if (sum(named) > 0)
    for (i in which(named))
      assign(names[i], eval(dots[[i]], env, env), env)

  with(env, {

    `class<-`(
      function(.) {

        `__self`[["."]] <- .

        passed <- vapply(`__conditions`, `__verify`, logical(1))

        if (!all(passed)) {

          failed <- unlist(vapply(`__conditions`[which(!passed)],
                                  function(cond) {
                                    custom <- attr(cond, "custom_msg")
                                    if (is.null(custom) || !is.character(custom))
                                      deparse(cond, nlines = 1L)
                                    else
                                      custom[1L]
                                  },
                                  character(1)))

          msg <- sprintf("conditions failed for call '%s':\n%s\n%s",
                         `__format_call`(sys.call(1L)),
                         paste(paste("\t *", failed), collapse = "\n"),
                         if (is.character(err_desc) && err_desc[1L] != "")
                           paste(" Description:", err_desc[1L])
                         else "")

          . <-
            if (is.function(fail_with)) {
              fail_with(simpleError(msg))
            } else {
              fail_with
            }
        }

        return(.)
      }, value = c("ensurer", "function")
    )
  })
}

#' Print method for ensurer contracts
#'
#' @param x a function made with \code{ensures_that}
#' @return x
#'
#' @export
print.ensurer <- function(x)
{
  cat("Ensures that\n")
  lapply(environment(x)[["__conditions"]],
         function(e) cat("\t*", deparse(e, nlines = 1L), "\n"))
  invisible(x)
}
