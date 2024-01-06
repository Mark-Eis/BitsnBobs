# BitsnBobs R Package
# Mark Eisler Jan 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# methods.R


# ========================================
#' @title
#' Info Attribute of {utils} Methods Function
#'
#' @description
#' Information on available methods for an S3 and S4 generic function, or all methods for an S3 or S4 class.
#'
#' @details
#' `method_info()` provides information on available methods for an S3 and S4 [`generic function`][base::InternalMethods],
#' or all methods for an S3 or S4 class obtained from the `"info"` attribute returned by [`methods()`][utils::methods],
#' which is more informative than the usual simple `character vector` printed output.
#'
#' The [`print_all()`][print_all] S3 method for class `MethodsFunction` works similarly and is used internally by
#' `method_info()`.
#'
#' @inherit utils::methods details
#'
#' @family methods
#' @seealso [`class`][base::class], [`getS3method`][utils::getS3method], [`methods()`][utils::methods],
#'   [`print_all.MethodsFunction()`][print_all] and [`UseMethod`][base::UseMethod]
#'
#' @inheritDotParams utils::methods generic.function class
#'
#' @inheritParams S3Gen_Meth
#'
#' @return A [`data.frame`][base::data.frame] with class attributes `c("info_df", "catapult", "data.frame")` and the
#'   following columns: -
#'
#' \item{Method}{`character vector` of the S3 method names obtained by pasting the generic function and class together.}
#'
#' \item{Visible}{`logical`, is the method exported from the namespace of the package in which it is defined?}
#'
#' \item{From}{`factor`, the location or package name where the method was found.}
#'
#' \item{Generic}{`character vector` of the names of the generic.}
#'
#' \item{isS4}{`logical`, true when the method is an S4 method.}
#'
#' @keywords methods
#' @export
#' @examples
#' 
#' methods(summary)
#' method_info(summary)
#' methods(summary) |> print_all()
#'
#' methods(class = "glm")
#' method_info(class = "glm")
#' method_info(class = "glm", .arrange_by = across(c(isS4, Method)))
#' methods(class = "glm") |> print_all()
#' methods(class = "glm") |> print_all(.arrange_by = across(c(isS4, Method)))
#' 

method_info <- function(..., .arrange_by = across(everything())) {
    methods(...) |> print_all(.arrange_by = .arrange_by)
}

# ========================================
#' @title
#' Tables of S3 Generic Functions and Methods
#'
#' @name S3Gen_Meth
#'
#' @description
#' `known_s3generics()` tabulates "known" S3 generic functions. 
#'
#' `s3_in_namespace()` tabulates S3 `methods` in a loaded `namespace`. 
#'
#' @details
#' `known_s3generics()` returns a `data.frame` of "known" [`S3 generic functions`][base::InternalMethods], based on two
#' `character vectors` in the \pkg{\link[base]{base}} package [`namespace`][base::loadNamespace], namely
#' [`.knownS3Generics`][base::asNamespace] and [`.S3PrimitiveGenerics`][base::.S3PrimitiveGenerics].
#'
#' `s3_in_namespace()` returns a `data.frame` of [`S3 methods`][base::UseMethod] in a loaded
#' [`namespace`][base::loadNamespace]. 
#'
#' @references
#' R Internals: \href{https://cran.r-project.org/doc/manuals/r-devel/R-ints.html#Namespaces}{\code{1.2.2 Namespaces}}.
#'
#' @family methods
#' @seealso [`.knownS3Generics`][base::asNamespace], [`.S3PrimitiveGenerics`][base::.S3PrimitiveGenerics]
#'   [`class`][base::class], [`getS3method`][utils::getS3method], [`loadNamespace`][base::loadNamespace],
#'   [`methods`][utils::methods] and [`UseMethod`][base::UseMethod].
#'
#' @param .arrange_by <[`data-masked`][rlang::args_data_masking]> names of columns or functions for ordering results
#'   using the syntax of \pkg{\link[dplyr]{dplyr}} [`arrange()`][dplyr::arrange]. Use [`desc()`][dplyr::desc] to sort
#'   by a variable in descending order.
#'
#' @param namespace (unquoted) name of a `namespace`.
#'
#' @return A [`data.frame`][base::data.frame] with class attributes `c("info_df", "catapult", "data.frame")` and columns
#'   `Generic` and `Namespace` for `known_s3generics()`, and `Generic`, `Class` and `Method` for `s3_in_namespace()`,
#'   as follows: -
#'
#' \item{Generic}{`character`, the generic function name.}
#'
#' \item{Namespace}{`character`, the namespace environment of the generic or `"primitive"` in the case of
#'   [`primitive`][base::primitive] (internally implemented) generics.}
#'
#' \item{Class}{`character`, the `class` for the method.}
#'
#' \item{Method}{`character`, the full method name.}
#'
#' @keywords internal methods
#' @export
#' @examples
#' known_s3generics()
#' known_s3generics(.arrange_by = across(Namespace:Generic))
#'
#' s3_in_namespace(BitsnBobs)
#' s3_in_namespace(BitsnBobs, .arrange_by = across(Class:Generic))
#'
#' s3_in_namespace(utils) |> head(40)
#' s3_in_namespace(utils, .arrange_by = across(Class:Generic)) |> head(40)
#'
 
known_s3generics <- function(.arrange_by = across(Generic:Namespace)) {
    gens <- c(.knownS3Generics, set_names(rep("\"primitive\"", length(.S3PrimitiveGenerics)), .S3PrimitiveGenerics))
    data.frame(Generic = names(gens), Namespace = gens) |>
        arrange({{.arrange_by}}) |>
        new_info_df(prt_str = "\"Known\" S3 Generic Functions")
}

# ========================================
#  Table of S3 Methods in a Namespace
#  s3_in_namespace
#'
#' @rdname S3Gen_Meth
#' @export

s3_in_namespace <- function(namespace, .arrange_by = Method) {
    namespace_name <- deparse(substitute(namespace))
    ns_S3methods <- getNamespace(namespace_name) |> getNamespaceInfo("S3methods")
    dimnames(ns_S3methods)[2] <- list(c("Generic", "Class", "Method", "Unknown"))
    as.data.frame(ns_S3methods[, -4]) |>
        arrange({{.arrange_by}}) |>
        new_info_df(prt_str = paste("S3 Methods in Namespace", namespace_name))
}

# ========================================
#  Constructor for an Information Data Frame
#  new_info_df()
#
# Not exported

new_info_df <- function(x = data.frame(NULL), prt_str = "Information Data Frame", ...) {
    stopifnot(is.data.frame(x))
    x <- catapult(x, prt_str)
    structure(x, class = c("info_df", class(x))) 
}

# ========================================
#  Print an Information Data Frame
#  S3method print.info_df()
#'
#' @rdname S3Gen_Meth
#' @export

print.info_df <- function(x, ...) {
    NextMethod(right = FALSE, row.names = FALSE)
}


# ========================================
#' @title
#' S3 Method Debugging Functions
#'
#' @name S3_Method_Debug
#'
#' @description
#' Functions intended for use in interactive mode from within the [`browser()`][base::browser].
#'
#' @details
#' `ls_all()` returns a [`character vector`][base::character] giving the names of all objects in its caller's
#' [`environment`][base::environment] including any that begin with a \sQuote{⁠.⁠}, and is convenient shorthand for
#' `ls(.all.names = TRUE)`, for instance when used from within the [`browser()`][base::browser] in interactive mode.
#'
#' Intended for use while debugging an S3 [`method`][utils::methods] in interactive mode with the
#' [`browser()`][base::browser], `s3mag7()` returns a named list of seven \sQuote{special} objects in the S3 method
#' dispatch `environment`, see the \strong{Technical Details} section of [`UseMethod`][base::UseMethod].
#'
#' `browse()` calls a specified function `fn` in "browser" mode with suitable arguments provided in \code{\dots}. The
#' \pkg{base} function [`debug`][base::debug] is generally preferable.
#' 
#' @family methods
#' @seealso [`browser()`][base::browser], [`class`][base::class], [`debug`][base::debug],
#'   [`environment`][base::environment], [`methods()`][utils::methods], [`parent.frame()`][base::parent.frame] and
#'   [`UseMethod`][base::UseMethod]
#'
#' @param env an `environment` to use in listing the available objects, equivalent to the `name` argument of
#'   [`ls()`][base::ls]; default [`parent.frame()`][base::parent.frame] i.e., the `environment` in which the function
#'   was called.
#'
#' @param \dots additional arguments passed to `ls()` or `fn()`.
#'
#' @param fn a `function` to be called in \dQuote{browser mode}.
#'
#' @inheritParams base::ls
#'
#' @return A [`character vector`][base::character] for `ls_all()`; for `s3mag7()`, a named [`list`][base::list]
#'   containing the following elements: -
#'
#' \item{`.Class`}{.Class is a  [`character vector`][base::character] of [`classes`][base::class] used to find the next
#'   [`method`][utils::methods]. [`NextMethod()`][base::NextMethod] adds an attribute `"previous"` to .Class giving
#'   the .Class last used for dispatch, and shifts .Class along to that used for dispatch.}
#'
#' \item{`.Generic`}{A length-one [`character vector`][base::character] naming the generic function for the current
#'   [`method`][utils::methods].}
#'
#' \item{`.GenericCallEnv`}{The environment of the call to be generic.}
#'
#' \item{`.GenericDefEnv`}{The environment defining the generic, used to find [`methods`][utils::methods] registered
#'   for the generic.}
#'
#' \item{`.Group`}{The generic [`group`][base::groupGeneric] to which the [`method`][utils::methods] belongs, if
#'    applicable.}
#'
#' \item{`.Method`}{A character vector (normally of length one) naming the [`method`][utils::methods] function. (For
#'   functions in the generic group [`Ops`][base::groupGeneric], it is of length two.)}
#'
#' \item{`"object"`}{i.e., the `"object"` comprising the first argument of the call to `.Generic`.}
#'
#' \item{`.class2("object")`}{The exact full [`character vector`][base::character] of the classes of `"object"` used by
#'   [`UseMethod()`][base::UseMethod].}
#' 
#' @keywords internal methods
#' @export
#' @examples
#' fn <- function() {
#'     m <- "Mimi"
#'     p <- "Poley"
#'     .b <- "Blossom"
#'     ls_all()
#' }
#'
#' fn()
#'
#' rm(fn)
#'
#' ## To run this in browser() interactive mode from R Console, select lines between
#' ##   "## Not run:" and "## End(Not run)" and hit [shift][enter]
#'
#' \dontrun{
#'
#' ## Two-by-two table for diagnostic test comparison
#' (twobytwo <- matrix(
#'     c(31, 12, 4, 58),
#'     nrow = 2, 
#'     dimnames = rep(list(c("+ve", "-ve")), 2) |>
#'         setNames(c("Test1", "Test2"))
#' ))
#'
#' browse(print_all, cohens_kappa(twobytwo))
#' s
#' where
#' ls_all()
#' s
#' where
#' ls_all()
#' s
#' where
#' ls_all()
#' s
#' where
#' ls_all()
#' s3mag7()
#' f
#'
#' rm(twobytwo)
#' }


ls_all <- function(all.names = TRUE, env = parent.frame(), ...) {
    lst <- ls(all.names = all.names, name = env)
    cat("ls(all.names = TRUE) : -\n\t")
    print(lst)
    cat("\n")
    invisible(lst)
}

# ========================================
#  Seven 'Special' Objects in the S3 Method Dispatch Environment
#
#' @export
#' @rdname S3_Method_Debug

s3mag7 <- function(env = parent.frame()) {
    sobs <- c(".Class", ".Generic", ".GenericCallEnv", ".GenericDefEnv", ".Group", ".Method") |>
    setNames(nm = _) |>
    map(\(x) env[[x]])
    sobs[[7]] <- formalArgs(match.fun(sobs$.Method))[1]
    sobs[[8]] <- .class2(env[[sobs[[7]]]])
    names(sobs)[7] <- paste(sobs$.Method, "Arg1")
    names(sobs)[8] <- paste0(".class2(", sobs[7], ")")
    sobs
}

# ========================================
#  Call a Function in "Browser" Mode
#
#' @export
#' @rdname S3_Method_Debug

browse <- function(fn, ...) {
	browser()
	fn(...)
}
