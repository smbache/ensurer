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
	dots   <- eval(substitute(alist(...)))
	names  <- names(dots)
	named  <- if (is.null(names)) rep(FALSE, length(dots)) else names != ""
	
	if (sum(!named) == 0)
		stop("At least one condition is needed for an ensurance.", call. = FALSE)
		
	env <- new.env(parent = parent.frame())
	env[["__self"]] <- env
	env[["__conditions"]] <- dots[!named]
	env[["fail_with"]] <- function(e) stop(e)
	
	if (sum(named) > 0)
		for (i in which(named))
			assign(names[i], eval(dots[[i]], env, env), env)
	
	with(env, {
		
		`__falsify` <- function(any.) FALSE
		`__verify`  <- function(cond) tryCatch(isTRUE(eval(cond)), 
                                           warning = `__falsify`, 
                                           error   = `__falsify`)
		
		function(.) {
			
			`__self`[["."]] <- .
			
			passed <- vapply(`__conditions`, `__verify`, logical(1))
			
			if (!all(passed)) {
				
				failed <- unlist(vapply(`__conditions`[which(!passed)], 
																deparse, 
																character(1),
																nlines = 1L))
				
				msg <- sprintf("The following condition(s) failed:\n%s\n", 											 
											 paste(paste("\t", failed), collapse = "\n"))
				
				. <- 
					if (is.function(fail_with)) 
						fail_with(simpleError(msg))
					else 
						fail_with
			}
			
			return(.)
		}
	})
}
