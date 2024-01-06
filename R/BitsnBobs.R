# BitsnBobs R Package
# Mark Eisler - Jan 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# BitsnBobs.R


# ========================================
#' Factor As Numeric
#'
#' Transform a factor to approximately its original numeric values.
#'
#' See \sQuote{Warning} section of [`factor`][base::factor]: \enc{–}{-}
#'
#' \dQuote{In particular, `as.numeric` applied to a factor is meaningless, and may happen by implicit
#' coercion. To transform a factor `f` to approximately its original numeric values,
#' `as.numeric(levels(f))[f]` is recommended and slightly more efficient than
#' `as.numeric(as.character(f))`.}
#'
#' Accordingly, `fct_to_num()` implements this method.
#'
#' @seealso  [`factor`][base::factor]
#'
#' @param f factor to be converted to numeric values
#'
#' @return Numeric
#'
#' @keywords category math
#' @export
#' @examples
#'  ## Create sequence of numeric values as a factor
#' f <- factor(2001:2020) |> print()
#'
#' ## Seemingly numeric levels are actually character strings!
#' levels(f) |> str()
#'
#' ## Return codes for factor levels, not what was expected
#' f |> as.numeric()
#'
#' ## Returns numeric values equivalent to factor levels
#' f |> fct_to_num()
#'
#' rm(f)

fct_to_num <- function(f) as.numeric(levels(f))[f]

# ========================================
#' Remove Sequentially Numbered Objects From Workspace
#'
#' Remove a series of sequentially named objects from the workspace or from another specified
#' environment. For example, conveniently remove a series of sequentially numbered models.
#'
#' `rm_objects()` lists all objects in the workspace (or another specified environment) whose
#' names start with `basename`, then removes any in which `basename` is followed by
#' an element included in `suffixes`, and finally lists all remaining objects with names
#' matching `basename`.
#'
#' @seealso [`ls`][base::ls] and [`rm`][base::rm].
#'
#' @param basename Common base name (quoted) of the series of objects.
#' @param suffixes A numeric or character vector representing the suffixes of the series of objects.
#' @param envir An environment from which to remove objects. Use `.GlobalEnv` for the workspace; default
#'   `caller_env()`.
#'
#' @return A character vector of matching names remaining in the workspace or another specified
#'   environment, returned invisibly.
#'
#' @keywords environment
#' @export
#' @examples
#'
#'  ## Create some sequentially numbered objects
#'  model1 <- model2 <- model3 <- model4 <- lm(1~1)
#'  ls(pattern = "model")
#'
#'  ## Remove three of them
#'  rm_objects(model, 1:3)
#'
#'  ## Create some sequentially named objects
#'  model_a <- model_b <- model_c <- model_d <- lm(1~1)
#'  ls(pattern = "model_")
#'
#'  ## Remove three of them
#'  rm_objects(model_, letters[1:3])
#'
#'  ## Use within a function - here it's anonymous but doesn't have to be
#'  (\() {
#'    model1 <- model2 <- model3 <- model4 <- model5 <- lm(1~1)
#'    rm_objects(model, 1:5)
#'  })()
#'
#'  ls(pattern = "model")
#'
#'  rm_objects(model, c(4, "_d"))

rm_objects <- function(basename, suffixes, envir = rlang::caller_env()) {
    basename <- enquo(basename)
    intro <- paste0("Objects matching \"", as_name(basename), "\u2026\"")
    envirname <- rlang::env_name(envir)
    envstr <- paste("in", ifelse(identical(envirname, ""), "this", envirname), "environment:\n\t")
    objs <- expr(ls(envir, pattern = as_name(basename)))

    cat(intro, envstr, eval(objs), "\n")
    rm(list = map_chr(suffixes, ~ paste0(as_name(basename), .)), envir = envir)
    cat(intro, "remaining", envstr, eval(objs), "\n")
    invisible(eval(objs))
}

# ========================================
#' @title
#' Stars For Statistical Significance
#'
#' @description
#' Stars for statistical significance with levels as usual in R. A vectorised function.
#' 
#' @param p A numeric vector of probabilities.
#' 
#' @return A character vector, length of `p`.
#' 
#' @export
#' @examples
#' (test_seq <- round(10 ^ seq(-4, 0, 0.5), 4))
#' 
#' starsig(test_seq)
#' 
#' rbind(test_seq, as.character(starsig(test_seq)))
#' 
#' data.frame(val = test_seq, sig = starsig(test_seq))
#' 
#' rm(test_seq)

starsig <- function(p) {
    if (!is.numeric(p))
        stop("p must be numeric") 

    cut(p, c(0, 0.001, 0.01, 0.05, 0.1, 1), include.lowest = T, right = F) |>
    `levels<-`(c("***", "**", "*", ".", "NS"))
}

