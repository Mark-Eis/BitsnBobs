# BitsnBobs R Package
# Mark Eisler - May 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# remove-objects.R


# ========================================
#' Remove Sequentially Numbered Objects from Workspace
#'
#' Remove a series of sequentially named objects from the workspace or from another specified
#' `environment`. For example, conveniently remove a series of sequentially numbered models.
#'
#' `rm_objects()` lists all objects in the workspace (or another specified  [`environment`][base::environment])
#' whose names start with `basename`, then removes any in which `basename` is followed by an element included in
#' `suffixes`, and finally lists all remaining objects with names matching `basename`.
#'
#' \acronym{#GF}
#'
#' @seealso [`environment`][base::environment], [`ls()`][base::ls] and [`rm()`][base::rm].
#'
#' @param basename Common base name (quoted or unquoted) of the series of objects.
#'
#' @param suffixes A numeric or character vector representing the suffixes of the series of objects.
#'
#' @param envir An [`environment`][base::environment] from which to remove objects. Use `.GlobalEnv` for the workspace;
#'   default [`parent.frame()`][base::sys.parent] i.e., the `environment` in which `rm_objects()` was called.
#'
#' @return A character vector of matching names remaining in the calling [`environment`][base::environment], usually
#'   the workspace unless `rm_objects()` was called within a function, or another specified `environment`, returned
#'   invisibly.
#'
#' @keywords environment
#' @export
#' @examples
#'  ## Note: running outside example() will be more informative
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
#'  ## Use within a function
#'  (\() {                  ## Anonymous function, but doesn't have to be
#'    model1 <- model2 <- model3 <- model4 <- model5 <- lm(1~1)
#'    rm_objects(model, 1:5)
#'  })()
#'
#'  ls(pattern = "model")
#'
#'  rm_objects(model, c(4, "_d"))

rm_objects <- function(basename, suffixes, envir = parent.frame()) {
    basename <- substitute(basename)
    if(!is.character(basename))
        basename <- deparse1(basename)
    intro <- paste0("Objects matching \"", basename, "\u2026\"")
    envirname <- if(identical(envir, globalenv())) "global" else environmentName(envir)
    if(!nchar(envirname))
        envirname <- "(unnamed)"
    envstr <- paste("in", envirname, "environment: \u2013\n\t")
    objs <- quote(ls(envir, pattern = basename))

    found <- eval(objs)
    cat(intro, "found", envstr, if(length(found)) found else "Zilch\u2014better luck next time!", "\n")
    if(length(found)) {
        rm(list = vapply(suffixes, \(x) paste0(basename, x), vector("character", 1)), envir = envir)
        found <- eval(objs)
        cat(intro, "remaining", envstr,  if(length(found)) found else "All gone!", "\n")
    }
    invisible(found)
}

