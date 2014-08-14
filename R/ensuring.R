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
	parent <- parent.frame()
	dots   <- eval(substitute(alist(...)))
	names  <- names(dots)
	named  <- if (is.null(names)) rep(FALSE, length(dots)) else names != ""
	
	dots[named] <- lapply(dots[named], eval, envir = parent, enclos = parent)
		
	if (sum(!named) == 0)
		stop("At least one condition is needed for an ensurance.", call. = FALSE)
		
	local({
		`__falsify__` <- function(any.) FALSE
		
		`__verify__`  <- function(cond, env) 
			tryCatch(isTRUE(eval(cond, env, env)), 
			  			 warning = `__falsify__`, 
							 error   = `__falsify__`)
		
		`__conditions__` <- dots[!named]
		
		fail_with <- function(e) stop(e)
		
		if (sum(named) > 0)
			for (i in which(named))
				assign(names[i], dots[[i]], environment())
		
		function(.) {
			passed <- 
				vapply(`__conditions__`, `__verify__`, logical(1), environment())
			
			if (!all(passed)) {
				
				failed <- unlist(vapply(`__conditions__`[which(!passed)], 
																deparse, 
																character(1),
																nlines = 1L))
				
				msg <- sprintf("The following condition(s) failed:\n%s\n", 											 
											 paste(paste("\t", failed), collapse = "\n"))
				
				. <- if (is.function(fail_with))
						   fail_with(simpleError(msg))
					   else
						   fail_with
			}
			return(.)
		}
	})
}