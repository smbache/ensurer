# Internal function for creating an environment for the ensures_that function.
#
# @param parent The environment to use as parent for the ensurer environment.
# @param fail_with A function or value to use on error.
# @param err_desc A character string with an additional error description.
# @return an environment used for evaluation of ensurer contracts.
ensurer_env <- function(parent, fail_with, err_desc)
{
  env <- new.env(parent = parent)
  env[["__self"]] <- env

  # error handler or return value on failure.
  env[["fail_with"]] <- fail_with

  # custom error description.
  env[["err_desc" ]] <- err_desc


  # utility function to format the call for the error description.
  env[["__format_call"]] <- format_call

  # utility function extract/create a condition's message.
  env[["__condition_message"]] <- condition_message

  # Function to return FALSE on any input.
  env[["__falsify"]] <- function(any.) FALSE

  # Function to veryfy a conditon.
  env[["__verify"]] <-
    function(cond) tryCatch(isTRUE(eval(cond, env, env)),
                            warning = env[["__falsify"]],
                            error   = env[["__falsify"]])

  # Return the environment.
  env
}
