# Internal function for creating an environment for the ensures_that function.
ensurer_env <- function(parent)
{
  env <- new.env(parent = parent)
  env[["__self"]] <- env

  # names which are meant to be replacable using named arguments.
  env[["fail_with"]] <- function(e) stop(e)
  env[["err_desc" ]] <- ""

  env[["__format_call"]] <- function(cl) {
    call_string <- deparse(cl, nlines = 1L)
    n <- nchar(call_string)
    if (n > 41) {
      snd_half <- substring(call_string, 19 + 5, n)
      n2 <- nchar(snd_half)
      paste(substring(call_string, 1, 18), "...",
            substr(snd_half, max(n2 - 17, 1), n2))
    } else {
      call_string
    }
  }

  env[["__falsify"]] <- function(any.) FALSE

  env[["__verify"]] <-
    function(cond) tryCatch(isTRUE(eval(cond, env, env)),
                            warning = env[["__falsify"]],
                            error   = env[["__falsify"]])

  env
}
