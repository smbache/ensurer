# Format Call for Output
#
# This internal function transforms a call into a short version for
# appropriate for the error message. It will estimate the width of the
# output based on \code{getOption("width")} when formatting the call.
#
# @param cl a call
#
# @return a character string
format_call <- function(cl)
{
  # deparse the call, and get its length
  call_string <- deparse(cl, nlines = 1L)
  n <- nchar(call_string)

  # Find an appropriate max width for the error message
  max_width   <- getOption("width")
  if (is.null(max_width) || max_width < 80)
    max_width <- 80

  # The max length of the variable part of the message
  max_length <- max_width - 38

  # Construct the message:
  if (n > max_length) {
    fst_half <- substring(call_string, 1,  floor(max_length/2) - 2)
    snd_half <- substring(call_string, n - floor(max_length/2) + 3, n)
    paste(fst_half, "..", snd_half)
  } else {
    call_string
  }
}

# Extract Message for a Condition.
#
# Some conditions have custom messages associated witg them. If so, this
# function will get this message. If it is missing, a message is created.
#
# @param cond A condition
#
# @return a character string.
condition_message <- function(cond) {
  custom <- attr(cond, "custom_msg")
  if (is.null(custom) || !is.character(custom))
    deparse(cond, nlines = 1L)
  else
    custom[1L]
}
