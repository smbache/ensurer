#' ensurer - Ensure Values at Runtime
#'
#' \code{ensurer} is a utility package for R that provides a simple and
#' light-weight mechanism for ensuring certain aspects of values at runtime.
#'
#' R does not provide any mechanism for type-safety and since it is not a
#' compiled language, the risk of having unexpected results is there at runtime.
#' R functions often accept different types for the same input and/or have
#' different return types for different sitations.
#'
#' As an example, a query to a database or the scraping of a website might not
#' return valid data, where "validity" can refer to a number of conditions. It
#' might be a positive or certain number of records; that all cases are
#' complete; that some column is weekly increasing; or simply that the result is
#' a \code{data.frame}.
#'
#' If one does not deal with these ambiguities and risks appropriately, some
#' resulting errors may be hard to track down or may even go unnoticed. It is
#' desirable to get an error immediately when a value does not have the correct
#' type or does not satisfy certain criteria.
#'
#' "Ensuring values" is here meant as a "contract", or a set of conditions, such
#' that if a value does not comply an error is raised instantly. An ensuring
#' contract (a function) is created with \code{\link{ensures_that}} (ideal for
#' multiple use or readability with complex contracts).
#'
#' It is also possible to ensure properties on the fly using
#' \code{\link{ensure_that}} (ideal for simple, one-time contracts).
#'
#' Using the \code{magrittr} pipe \code{\%>\%} greatly improves semantics of the
#' functionality provided by this package, but it is not necessary.
#'
#' This package is not meant as a substitute for unit testing, and great
#' packages for this already exist, e.g. \code{testthat} by Hadley Wickham. The
#' \code{ensurer} package is meant as a simple and ideal tool for scripts or
#' programs where runtime conditions may break the functionality, and where
#' errors should be raised as soon and clear as possible.
#'
#' For a more thorough introduction, see \code{vignette(ensurer)}.
#'
#' @docType package
#' @name ensurer
#' @author Stefan Holst Milton Bache <stefan@@stefanbache.dk>
NULL
