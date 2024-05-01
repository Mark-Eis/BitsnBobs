# BitsnBobs R Package
# Mark Eisler - May 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# powerseq.R

# ========================================
#' Expand Expression as a Power Sequence
#'
#' Create a power sequence from an expression, \var{base_expr}, and the maximum power number, \var{n}.
#'
#' The power sequence is returned as a \code{"call"} object, which may be evaluated using \code{\link[rlang]{eval_tidy}}
#' in package \pkg{\link[rlang]{rlang}}, see examples. By default, \code{power_seq()} returns a simple \code{call}
#' object containing just one instance of both \var{base_expr} and \var{n}.
#'
#' Alternatively, if \var{type} is \code{"evaluate"}, \code{power_seq()} evaluates \var{base_expr} and assigns the result
#' to a constant nested \var{n} times within the \code{call} object to be returned. This results in \var{base_expr} being
#' evaluated on execution of \code{power_seq()} rather than on evaluation of the returned call, which may be preferable
#' for more complex expressions.
#'
#' Finally, if \var{type} is \code{"replicate"}, \code{power_seq()} captures the \var{base_expr} argument and its
#' environment as a \code{\link[rlang]{quosure}} to be replicated \var{n} times within the returned nested \code{call}
#' object. This results in \var{base_expr} being evaluated \var{n} times on evaluation of the returned call; this multiple
#' evaluation may be acceptable in simple cases but might be slow and inefficient for complex expressions.
#'
#' @seealso \code{\link{call}}, \code{\link[rlang]{eval_tidy}} and \code{\link[rlang]{quosure}}.
#' @family powerseq
#'
#' @param base_expr a bare (quoted) expression, see examples.
#' @param n a non-negative integer or number coercible into a positive integer.
#' @param type a character string, (partially) matching one of \code{"simple"}, \code{"evaluate"} or \code{"replicate"};
#'   default \code{"simple"}.
#'
#' @return A \code{"call"} object i.e., a captured function call, possibly nested \var{n} times.
#'
#' @export
#' @examples
#' (pseq <- power_seq(a + b, 3))
#' ## pseq is a "call" object
#' typeof(pseq)
#' class(pseq)
#' mode(pseq)
#' is.call(pseq)
#' as.list(pseq)
#' is.call(pseq[[2]])
#' as.list(pseq[[2]])
#' is.call(pseq[[2]][[3]])
#' as.list(pseq[[2]][[3]])
#'
#' ## View the abstract syntax tree - requires {lobstr} package
#' if (!requireNamespace("lobstr", quietly = TRUE)) 
#'   warning("package 'lobstr' must be installed")
#' try(lobstr::ast(!!pseq))
#'
#' (pseq2 <- power_seq(log(x), 5))
#'
#' x <- 3
#' eval_tidy(pseq2)  ## Uses x from the global environment
#'
#' x <- 5
#' eval_tidy(pseq2)
#'
#' rm(x)
#' try(eval_tidy(pseq2))
#'
#' foo <- function() {
#'   x <- 10
#'   power_seq(log(x), 5)
#' }
#'
#' pseq2 <- foo()
#' pseq2                 ## Expression looks just the same but …
#'
#' x <- 3
#' eval_tidy(pseq2)  ## Consistently uses x from the environment of foo()
#'
#' x <- 5
#' eval_tidy(pseq2)
#'
#' rm(x)
#' eval_tidy(pseq2)
#'
#' ## Wrapper for log() reporting its execution using marker()
#' log <- function(...) {
#'   marker(msg = "in BitsnBobs Help")
#'   base::log(...)
#' }
#'
#' ## Compare the three options for type 
#' ## log() invoked just once, on execution of power_seq() with type = "evaluate"
#' (expr_ls <- c("simple", "evaluate", "replicate") |> setNames(nm = _) |>
#'     lapply(\(x) power_seq(log(3), 5, x)))
#'
#' ## log() invoked once on evaluation of expression from power_seq() with type = "simple" and
#' ## five times on evaluation of expression from power_seq() with type = "replicate"
#' (res_ls <- expr_ls |> lapply(eval_tidy))
#'
#' ## All three types evaluate identically: -
#' all(
#'   identical(res_ls[[1]], res_ls[[2]]),
#'   identical(res_ls[[1]], res_ls[[3]]),
#'   identical(res_ls[[2]], res_ls[[3]])
#' )
#'
#' ## Compare the three abstract syntax trees
#' try(expr_ls |> lapply(\(x) lobstr::ast(!!x)))
#'
#' rm(expr_ls, foo, log, pseq, pseq2, res_ls)

power_seq <- function(base_expr, n, type = c("simple", "evaluate", "replicate")) {
	type <- match.arg(type)
	if(type %in% c("simple", "replicate"))
		base_expr <- enquo(base_expr)
	if(type %in% c("evaluate", "replicate")) {
	    new_expr <- base_expr
	    lapply(seq_len(n)[-1], \(x){
	        new_expr <<- expr(!!new_expr + (!!base_expr)^!!x)
	    })
	    new_expr
	} else {
		if(type == "simple")
 			expr(`^`(!!base_expr, seq_len({{n}})) |> sum())
		else 
			stop("type argument must be \"simple\", \"evaluate\" or \"replicate\"!")
	}		
} 


# ========================================
#' Expand Term in Right Hand Side of a Formula as a Power Sequence
#'
#' Create a power sequence from a term in a \code{\link[stats]{formula}}, \var{base_fla}, and the maximum power number,
#' \var{n}, optionally including other formula terms.
#'
#' @seealso \code{\link[stats]{formula}}.
#' @family powerseq
#'
#' @param base_fla the a term in the formula to be expanded as a power sequence.
#' @param ... additional terms in the formula.
#' @inheritParams power_seq
#'
#' @return A \code{formula}.
#'
#' @export
#' @examples
#' formul_pwrseq(y ~ x, 5)
#' formul_pwrseq(y ~ log(x), 5)
#' formul_pwrseq(y ~ A, 3, B, C, D)

formul_pwrseq <- function(base_fla, n, ...) {
	new_fla <- base_fla
	lapply(seq_len(n)[-1], \(x) {f_rhs(new_fla) <<- expr(!!f_rhs(new_fla) + I(`^`(!!f_rhs(base_fla), !!x)))})
	if (...length())
		enexprs(...) |> lapply(\(x) {f_rhs(new_fla) <<- expr(!!f_rhs(new_fla) + !!x)})
	new_fla
}
