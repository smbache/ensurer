#' Integer type
#'
#' @param . Object to be tested for being of integer type.
#' @return identity
type_integer <-
ensures(is.vector(., "integer") ~ "Value is not an integer (vector)")

#' Whole number type
#'
#' @param . Object to be tested for being a whole number.
#' @return identity
type_whole <-
ensures(is.vector(., "numeric") ~ "Value is not a numeric (vector)",
             all(floor(.) == .) ~ "Values are not whole numbers.")

#' Whole number
#'
#' @param . Object to be tested for being of character type.
#' @return identity
type_character <-
ensures(is.vector(., "character") ~ "Value is not a character (vector)")

#' Numeric type
#'
#' @param . Object to be tested for being of numeric type.
#' @return identity
type_numeric <-
  ensures(is.vector(., "numeric") ~ "Value is not a numeric (vector)")

#' Type of something
#'
#' @param . Object
#' @return identity
type_some <-
ensures(identical(., .))
