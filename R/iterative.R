# BitsnBobs R Package
# Mark Eisler - Jul 2023
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# BitsnBobs.R

# ========================================
#' Generic Iterative And Recursive Function Handlers
#'
#' Iterate or recur a function until the result converges.
#'
#' Iterates or recurs the function provided until the result converges at a given tolerance, assessed
#' by testing changes in the value (rounded to a given number of number of decimal places) at each
#' iteration or recurrence.
#'
#' Although the iterative or recursive function should have just one argument, it may contain additional constant
#' values or data, see examples.
#'
#' \code{\link{iterate}} is similar to \code{\link{recursive}} but uses a while loop rather than recursion.
#'
#' @seealso \code{\link[base]{options}}, \code{\link[base]{options}("digits")}.
#'
#' @param x an initial estimate to be passed to the iterative function.
#' @param func the iterative function, which should have just one argument representing the previous
#' best estimate.
#' @param tolerance the number of decimal places to which the revised estimate is tested against its predecessor
#' for convergence; default \code{\link[base]{options}("digits")}.
#'
#' @return Result of iteration or recursion of the function.
#'
#' @keywords iteration programming
#' @export
#' @examples
#' ## Newton's method for square root 1000 using a shorthand form anonymous function
#' iterate(30, \(y)((y + 1000 / y) / 2))
#' recursive(30, \(y)((y + 1000 / y) / 2))
#' 
#' ## Newton's method for any square root using a function defined within a function
#' newtroot <- function(x, est = x / 2) {
#' 	 func <- function(y) (y + x / y) / 2
#' 	 iterate(est, func)
#' }
#'
#' newtroot(1000)
#' newtroot(1000, 30)
#'
#' newtroot <- function(x, est = x / 2) {
#' 	 func <- function(y) (y + x / y) / 2
#' 	 recursive(est, func)
#' }
#'
#' newtroot(1000)
#' newtroot(1000, 30)
#'
#' ## More directly using a shorthand form anonymous function within a function
#' newtroot <- function(x, est = x / 2)
#' 	 iterate(est, \(y)((y + x / y) / 2))
#'
#' newtroot(1000)
#'
#' newtroot <- function(x, est = x / 2)
#' 	 recursive(est, \(y)((y + x / y) / 2))
#'
#' newtroot(1000)
#'
#' ##  Build in greater precision using tolerance argument
#' ##  - albeit not seen without changing options("digits")
#' newtroot <- function(x, est = x / 2)
#' 	 iterate(est, \(y)((y + x / y) / 2), tolerance = 15)
#'
#' newtroot(1000)
#'
#' newtroot <- function(x, est = x / 2)
#' 	 recursive(est, \(y)((y + x / y) / 2), tolerance = 15)
#'
#' newtroot(1000)
#'
#' ##  Build in less precision using tolerance argument
#' ##  - not easily seen without changing options("digits")
#' newtroot <- function(x, est = x / 2)
#' 	 iterate(est, \(y)((y + x / y) / 2), tolerance = 0)
#'
#' newtroot(1000)
#'
#' ##  - More easily seen without changing options("digits")
#' newtroot <- function(x, est = x / 2)
#' 	 recursive(est, \(y)((y + x / y) / 2), tolerance = 0)
#'
#' newtroot(1000)
#'
#' ## Changing options("digits") also gives greater precision by default
#' dig <- options(digits = 15)
#' 
#' newtroot <- function(x, est = x / 2)
#' 	 iterate(est, \(y)((y + x / y) / 2))
#'
#' newtroot(0.001, .03)
#'
#' newtroot <- function(x, est = x / 2)
#' 	 recursive(est, \(y)((y + x / y) / 2))
#'
#' newtroot(0.001, .03)
#'
#' options(digits = dig$digits)
#' rm(dig)

iterate <- function(x, func, tolerance = getOption("digits")) {
	
	y <- func(x)
	while (!identical(round(x, tolerance), round(y, tolerance))) {
		x <- y
		y <- func(x)
	}
	y
}

# ========================================
#  Generic Recursive Function Handler
#' @rdname iterate
#' @export

recursive <- function(x, func, tolerance = getOption("digits")) {
	
	y <- func(x)
	if (identical(round(x, tolerance), round(y, tolerance)))
		x
	else
		recursive(y, func, tolerance)
}
