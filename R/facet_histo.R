# BitsnBobs R Package
# Mark Eisler - May 2023
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# facet-histo.R

# ========================================
#' @title
#' Faceted Histogram
#'
#' @description
#' Produce neatly formatted histograms for a numeric variable in a data frame, faceted by one or more
#' categorical variables.
#'
#' @details
#' Uses the \pkg{ggplot2} package. Formatting of titles etc. is deliberately minimal so that the user can set
#' their own preferences as shown in the examples. The \code{\dots} argument may be omitted to obtain a simple unfaceted
#' histogram. A set of variables or expressions defining faceting groups may be quoted using \code{\link[ggplot2]{vars}}
#' and injected into the \code{\dots} argument with the \pkg{rlang} \code{\link[rlang]{!!!}} splice-operator, see examples.
#'
#' Categorical variables defining the faceting groups in the \code{\dots} argument must be \code{factor}s or character
#' vectors that will be coerced to \code{factor} using \code{\link{as.factor}}. If not supplied in the argument
#' \code{.bins}, the number of bins for the histograms is calculated as the square root of the total number of observations
#' divided by the product of the numbers of levels of the variables defining the faceting groups. 
#'
#' @seealso \code{\link[ggplot2]{facet_wrap}}, \code{\link[ggplot2]{ggplot}}, \code{\link[graphics]{hist}},
#' \code{\link[MASS]{truehist}} and \code{\link[ggplot2]{vars}}.
#'
#' @param .data a data frame, or a data frame extension (e.g. a tibble).
#'
#' @param x the quoted name of a \code{numeric} variable in \code{.data} to be plotted.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> quoted names of one or more \code{factors} or character vectors in
#'   \code{.data} defining faceting groups.
#'
#' @param .main a character string for the main plot title; default \samp{Histogram of} followed by the name of \var{x}.
#'
#' @param .sub a character string for the plot subtitle; default the total number of observations and the number of
#'   levels of each faceting variable.
#'
#' @param .xtitle a character string for the x-axis title; default the name of \var{x}.
#'
#' @param .col a character string for the fill colour; default \code{"steelblue2"}.
#'
#' @param .bins \code{integer} the number of bins for the histogram; default \code{NULL}.
#'
#' @return A \code{\link[ggplot2]{ggplot}}.
#'
#' @keywords hplot
#' @export
#' @examples
#' ## Using cabbages dataset from {MASS} package
#' \dontshow{
#'    if (!requireNamespace("MASS", quietly = TRUE)) 
#'        warning("package 'MASS' must be installed")
#'    try(cabbages <- MASS::cabbages)
#' }
#'
#' ## Without faceting variables
#' cabbages |> facet_histo(VitC)
#' ## One faceting variable
#' cabbages |> facet_histo(VitC, Date)
#' ## Customise titles
#' cabbages |> facet_histo(VitC, Cult, .main = "Vitamin C levels by Cultivar", .xtitle = "Ascorbic acid content")
#'
#' ## Set ggplot preferences
#' oldtheme <- theme_get()
#' theme_update(
#'    plot.title = element_text(color = "black", size = 20, hjust = 0.5),
#'    plot.subtitle = element_text(color = "black", size = 18, hjust = 0.5),
#'    axis.title.x = element_text(color = "black", size = 15),
#'    axis.title.y = element_text(color = "black", size = 15),
#'    legend.position = "none"
#' )
#'
#' ## Two faceting variables
#' cabbages |> facet_histo(
#'                 VitC, Cult, Date,
#'                 .main = "Vitamin C levels by Date and Cultivar",
#'                 .xtitle = "Ascorbic acid content",
#'                 .col = "seagreen2"
#'             )
#'
#' ## Unquote-splice a list of faceting variables
#' fvars <- ggplot2::vars(Date, Cult)
#'
#' cabbages |> facet_histo(
#'                 VitC,
#'                 !!!fvars,
#'                 .main = "Vitamin C levels by Cultivar and Date",
#'                 .xtitle = "Ascorbic acid content",
#'                 .col = "tomato2"
#'             )
#'
#' ## Retrieve plot data for the simple case without faceting
#' cabbages |> facet_histo(VitC) |>
#'     ggplot2::ggplot_build() |> _$data[[1]] 
#'
#' ## Retrieve the histogram bins - PANEL indicates for which facet
#' cabbages |> facet_histo(VitC, Cult) |>
#'     ggplot2::ggplot_build() |> _$data[[1]] |>
#'     dplyr::select(PANEL, xmin, xmax, count) 
#'
#' ## Restore ggplot settings
#' theme_set(oldtheme)
#' \dontshow{
#'     rm(cabbages, fvars, oldtheme)
#' }
#'

facet_histo <- function(.data, x, ..., .main, .sub, .xtitle, .col = "steelblue2", .bins = NULL) {
    x <- enquo(x)
    facet_by <- enquos(..., .named = TRUE)
    alldotsfactors <- expr(all(is.factor(c(!!!facet_by))))
    if (all(length(facet_by), !eval_tidy(alldotsfactors, data = .data))) {
        warning("Coercing one or more ... arguments to factors")
        .data <- mutate(.data, across(c(...), as.factor))
        if (!eval_tidy(alldotsfactors, data = .data))
            stop("All ... arguments must be factors or coercible to factors by as.factor()")
    }
    n <- nrow(.data)
    n_levels <- list(!!!facet_by) |> map_int(nlevels) |> expr() |> eval_tidy(.data)
    if(missing(.main))
        .main <- paste("Histogram of", as_label(x) |> str_to_title())
    if(missing(.sub)) {
        .sub <- paste0("n = ", n)
        if(length(n_levels))
            .sub <- paste0(.sub, ", n levels: ", paste(names(n_levels), n_levels, sep = " = ", collapse = ", "))
    }
    if(missing(.xtitle))
        .xtitle <- as_label(x) |> str_to_title()
    strip_text_size <- if (prod(n_levels) > 15) 9 else if (prod(n_levels) > 9) 11 else 14 
    strip_mrgn <- if (prod(n_levels) > 15) 1 else if (prod(n_levels) > 9) 2 else 3 
    ggplot(.data, aes(!!x)) +
    geom_histogram(
        bins = .bins %||% sqrt(n / prod(n_levels)) |> ceiling(),
        boundary = 0,
        closed = "left",
        color = "black",
        fill = .col
    ) +
    theme(
        axis.text.x = element_text(color = "black", size = 10, hjust = 0.5),
        axis.text.y = element_text(color = "black", size = 10, hjust = 0.5),
        strip.text.x = element_text(
            color = "black",
            size = strip_text_size,
            margin = margin(t = strip_mrgn, b = strip_mrgn)
        )
    ) +
    labs(
        x = .xtitle,
        title = .main,
        subtitle = .sub
    ) + {
	if (length(n_levels))
        facet_wrap(
            facet_by,
            scales = "free"
        )
    }
}
