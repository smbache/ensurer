#' Make a contract ensuring certain properties of a value.
#'
#' @param ... conditions which must pass for the ensuring contract to be 
#'        fulfilled. Any named argument will treated as values available 
#'        when evaluating the conditions. To reference the value itself
#'        use the dot-placeholder, `.`
#' @return an ensuring function
#' @rdname ensuring
#' @examples
#' \dontrun{
#' library(magrittr)
#' 
#' # Ensures a matrix is square. Read `<-` as "is".
#' ensure_square <- ensuring(NCOL(.) == NROW(.))
#' 
#' diag(4) %>%
#'   ensure_square
#' }
#' @export
ensuring <- function(...)
{
	dots  <- eval(substitute(alist(...)))
	names <- names(dots)
	named <- if (is.null(names)) rep(FALSE, length(dots)) else names != ""
	
	if (sum(!named) == 0)
		stop("At least one condition is needed for an ensurance.", call. = FALSE)
		
	local({
		if (sum(named) > 0)
			for (i in which(named))
				assign(names[i], eval(dots[[i]]), environment())
		
		`__conditions__` <- dots[!named]
		
		function(.) {
			conditions <- lapply(`__conditions__`, eval, environment())
			passed     <- vapply(conditions, isTRUE, logical(1))
			if (!all(passed)) {
				
				failed <- unlist(vapply(`__conditions__`[which(!passed)], 
																deparse, 
																character(1),
																nlines = 1L))
				
				msg <- sprintf("The following condition(s) failed:\n%s\n", 
											 
											 paste(paste("\t", failed), collapse = "\n"))
				stop(msg, call. = FALSE)
			}
			.
		}
	})
}