#' @rdname ensures_that
#' @export
ensure_that <- function(value., ..., fail_with = function(e) try(stop(e), T), err_desc = "")
{

  # if value. has class 'try-error', simply make the return value
  if (class(value.) == 'try-error') return(value.)
  # purpose: while chaining several ensure() calls with the '%>%' piping
  #          operator, at the end of the pipe - in case an error occures - the
  #          try-error of the actual failing pipe segment will become visible,
  #          see example below, it clarifies the usefulness in cases you mix
  #          operations and ensure commands within a single pipe

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

# ### EXAMPLE (compare output of last line of code with original ensure())
#
# if (!require(magrittr)) {
#   install.packages("magrittr")
#   library(magrittr)
# }
#
# 1:10 %>%                                  # take a numeric vector
#   as.character() %>%                      # convert it to character
#   ensure(is.character) %>%                # and ensure character type: OK!
#   as.numeric() %>%                        # convert to numeric
#   ensure(mean(., na.rm = TRUE) == 1) %>%  # check for unlikely result: NOT OK!
#
#   ensure(is.numeric)                      # actually gets to this line, because
#                                           # try-error was passed down the pipe
#                                           # instead of aborting expression AND
#                                           # value shows the line of the error
#
# # that is different than WITH THE ORIGINAL FUNCTION, even when changing
# # fail_withs arg from stop-error to try error in each ensure():
#
# # restoring original ensure()
# ensure_that <- function(value., ...,
#                         fail_with = function(e) stop(e), err_desc = "") {
#
#   parent <- parent.frame()
#   dots   <- eval(substitute(alist(...)))
#   e      <- eval(as.call(c(quote(ensures_that), dots,
#                            fail_with = fail_with,
#                            err_desc = err_desc)),
#                  parent, parent)
#
#   e(value.)
# }
#
# 1:10 %>%                                  # take a numeric vector
#   as.character() %>%                      # convert it to character
#   ensure(is.character,
#          fail_with = function(e)
#            try(stop(e), T)) %>%           # and ensure character type: OK!
#   as.numeric() %>%                        # convert to numeric
#   ensure(mean(., na.rm = TRUE) == 1,
#          fail_with = function(e)
#            try(stop(e), T)) %>%           # check for unlikely result: NOT OK!
#
#   ensure(is.numeric,                      # output is not the error of the
#          fail_with = function(e)          # line it initially occured
#            try(stop(e), T))
