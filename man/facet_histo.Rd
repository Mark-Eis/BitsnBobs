% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/facet_histo.R
\name{facet_histo}
\alias{facet_histo}
\title{Faceted Histogram}
\usage{
facet_histo(
  .data,
  x,
  ...,
  .main,
  .sub,
  .xtitle,
  .col = "steelblue2",
  .bins = NULL
)
}
\arguments{
\item{.data}{a data frame, or a data frame extension (e.g. a tibble).}

\item{x}{the quoted name of a \code{numeric} variable in \code{.data} to be plotted.}

\item{...}{<\code{\link[rlang:dyn-dots]{dynamic-dots}}> quoted names of one or more \code{factors} or character vectors in
\code{.data} defining faceting groups.}

\item{.main}{a character string for the main plot title; default \samp{Histogram of} followed by the name of \var{x}.}

\item{.sub}{a character string for the plot subtitle; default the total number of observations and the number of
levels of each faceting variable.}

\item{.xtitle}{a character string for the x-axis title; default the name of \var{x}.}

\item{.col}{a character string for the fill colour; default \code{"steelblue2"}.}

\item{.bins}{\code{integer} the number of bins for the histogram; default \code{NULL}.}
}
\value{
A \code{\link[ggplot2]{ggplot}}.
}
\description{
Produce neatly formatted histograms for a numeric variable in a data frame, faceted by one or more
categorical variables.
}
\details{
Uses the \pkg{\link[ggplot2]{ggplot2}} package. Formatting of titles etc. is deliberately minimal so that the
user can set their own preferences as shown in the examples. The \code{\dots} argument may be omitted to obtain
a simple unfaceted histogram. A set of variables or expressions defining faceting groups may be quoted using
\code{\link[ggplot2]{vars}} and injected into the \code{\dots} argument with the \pkg{\link[rlang]{rlang}}
\code{\link[rlang]{!!!}} splice-operator, see examples.

Categorical variables defining the faceting groups in the \code{\dots} argument must be \code{factor}s or character
vectors that will be coerced to \code{factor} using \code{\link{as.factor}}. If not supplied in the argument
\code{.bins}, the number of bins for the histograms is calculated as the square root of the total number of
observations divided by the product of the numbers of levels of the variables defining the faceting groups.
}
\examples{
## Using cabbages dataset from {MASS} package
\dontshow{
   if (!requireNamespace("MASS", quietly = TRUE)) 
       warning("package 'MASS' must be installed")
   try(cabbages <- MASS::cabbages)
}

## Without faceting variables
cabbages |> facet_histo(VitC)
## One faceting variable
cabbages |> facet_histo(VitC, Date)
## Customise titles
cabbages |> facet_histo(
                VitC, Cult,
                .main = "Vitamin C levels by Cultivar",
                .xtitle = "Ascorbic acid content"
            )

## Set ggplot preferences
oldtheme <- theme_get()
theme_update(
   plot.title = element_text(color = "black", size = 20, hjust = 0.5),
   plot.subtitle = element_text(color = "black", size = 18, hjust = 0.5),
   axis.title.x = element_text(color = "black", size = 15),
   axis.title.y = element_text(color = "black", size = 15),
   legend.position = "none"
)

## Two faceting variables
cabbages |> facet_histo(
                VitC, Cult, Date,
                .main = "Vitamin C levels by Date and Cultivar",
                .xtitle = "Ascorbic acid content",
                .col = "seagreen2"
            )

## Unquote-splice a list of faceting variables
fvars <- ggplot2::vars(Date, Cult)

cabbages |> facet_histo(
                VitC,
                !!!fvars,
                .main = "Vitamin C levels by Cultivar and Date",
                .xtitle = "Ascorbic acid content",
                .col = "tomato2"
            )

## Retrieve plot data for the simple case without faceting
cabbages |> facet_histo(VitC) |>
    ggplot2::ggplot_build() |> _$data[[1]] 

## Retrieve the histogram bins - PANEL indicates for which facet
cabbages |> facet_histo(VitC, Cult) |>
    ggplot2::ggplot_build() |> _$data[[1]] |>
    dplyr::select(PANEL, xmin, xmax, count) 

## Restore ggplot settings
theme_set(oldtheme)
\dontshow{
    rm(cabbages, fvars, oldtheme)
}

}
\seealso{
\code{\link[ggplot2]{facet_wrap}}, \code{\link[ggplot2]{ggplot}}, \code{\link[graphics]{hist}},
\code{\link[MASS]{truehist}} and \code{\link[ggplot2]{vars}}.
}
\keyword{hplot}
