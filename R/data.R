# BitsnBobs R Package
# Mark Eisler - March 2023
# For general bits and bobs of code
#
# BitsnBobs R Package
# Mark Eisler - May 2023
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# data.R

#' Starwars Dataset with Separated First Names and Surnames
#'
#' @description
#' Starwars dataset from \pkg{dplyr} with separated first names and surnames.
#'
#' @format ## `starwars2`
#' A data frame with 63 rows and 12 columns:
#' \describe{
#'   \item{Firstname}{Firstname of the character}
#'   \item{Surname}{Surname of the character}
#'   \item{height}{height}
#'   \item{mass}{mass}
#'   \item{height}{height}
#'   \item{hair_color}{hair color}
#'   \item{skin_color}{skin color}
#'   \item{eye_color}{eye color}
#'   \item{birth_year}{year of birth}
#'   \item{sex}{sex}
#'   \item{gender}{gender}
#'   \item{homeworld}{homeworld}
#'   \item{species}{species}
#' }

#' @source <https://dplyr.tidyverse.org/reference/starwars.html>
"starwars2"

#' Starwars Dataset with Name and Skin Colour Only
#'
#' @description
#' Starwars dataset from \pkg{dplyr} with just two columns.
#'
#' @format ## `starwars3`
#' A data frame with 10 rows and 2 columns:
#' \describe{
#'   \item{name}{Name of the character}
#'   \item{skin_color}{skin color}
#' }

#' @source <https://dplyr.tidyverse.org/reference/starwars.html>
"starwars3"

#' University Student Heights
#'
#' @description
#' Heights of 100 randomly selected male university students from Table 2.1 of Spiegel and Stephens (2008).
#'
#' @format ## `heights`
#' An \code{integer vector} with length \code{100}.
#' \describe{
#'   \item{heights}{Height of the student in inches.}
#' }

#' @source
#' Adapted from Spiegel, Murray R., and Larry J. Stephens, (2008). \emph{Theory and Problems of Statistics}.
#' 4th edn. McGraw-Hill.
#' \href{http://www.buders.com/UNIVERSITE/Universite_Dersleri/istatistik/statistics.pdf}{ISBN:9780071755498}
#'
#' See also: \href{https://brownmath.com/stat/shape.htm#Example1}{https://brownmath.com} 
"heights"

#' Rat Litter Sizes
#'
#' @description
#' Litter sizes in albino rats (n = 815) from Table 7 of King (1924).
#'
#' @format ## `litter_sizes`
#' An \code{integer vector} with length \code{815}.
#' \describe{
#'   \item{litter_sizes}{Sizes of albino rat litters.}
#' }

#' @source
#' King, H.D. (1924). Litter production and the sex ratio in various strains of rats. \emph{The Anatomical Record},
#' \strong{27}(5), 337-366. \href{https://doi.org/10.1002/ar.1090270514}{\doi{10.1002/ar.1090270514}}
#'
#' See also: \href{https://brownmath.com/stat/shape.htm#Example2}{https://brownmath.com} 
"litter_sizes"
