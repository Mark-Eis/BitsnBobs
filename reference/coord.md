# Geographic or GPS Coordinate Class

`coord()` creates a robust representation of a geographic or GPS
cordinate based on the value of `deg`, `min` and `sec`, instatiated as
an object of class `"coord"`.

`as_coord()` converts the format of geographic or GPS coordinates
between (i) decimal degrees, (ii) degrees and minutes, and (iii)
degrees, minutes and seconds. It also creates `"coord"` objects directly
from single numeric values in one of these three formats.

## Usage

``` r
coord(deg, min = NULL, sec = NULL, .latorlon = c(NA, "lat", "lon"))

as_coord(object, ...)

# S3 method for class 'coord'
as_coord(object, ..., .fmt = c("decdeg", "degmin", "degminsec"))

# S3 method for class 'numeric'
as_coord(
  object,
  ...,
  .fmt = c("decdeg", "degmin", "degminsec"),
  .latorlon = c(NA, "lat", "lon")
)

# S3 method for class 'waypoint'
as_coord(object, ..., .fmt = c("decdeg", "degmin", "degminsec"))
```

## Arguments

- deg:

  `numeric`, representing the number of degrees. Must be of type
  `integer` if `min` or `sec` are provided, otherwise type `double`;
  default `0L`.

- min:

  `numeric`, representing the number of minutes. If `sec` provided, must
  be of type `integer`, otherwise `double`; default `NULL`.

- sec:

  `double`, representing the number of seconds; default `NULL`.

- .latorlon:

  a `character` string, either `"lat"` or `"lon"` indicating whether the
  coordinate represented is of latitude or longitude, or `NA` (the
  default).

- object:

  a `"coord"` object or a `numeric` vector to be converted to another
  format.

- ...:

  further arguments passed to or from other methods.

- .fmt:

  `character` string indicating the desired format; must be one of
  `"decdeg"` (default), `"degmin"` or `"degminsec"`.

## Value

An object of class `"coord"` instantiating a coordinate. Objects of
`"coord"` class contain a `list` with one, two or three `numeric` values
named `"deg"`, `"min"`, `"sec"`, depending on whether the cordinate in
question is represented in decimal degrees, in (integer) degrees and
(decimal) minutes, or else in (integer) degrees, (integer) minutes, and
(decimal) seconds.

`"coord"` objects have `character` attribute `latorlon`, which may be
`"lat"` for latitude, `"lon"` for longitude or `NA`, and a `logical`
attribute `"negative"`, which when `TRUE` signifies a negative
coordinate i.e., S or W, rather than N or E.

## Details

The value provided in argument `deg` should have a decimal point after
the number of whole degrees in the case of decimal degrees. Likewise,
the value provided in argument `min` should have a decimal point after
the number of whole minutes in the case of degrees and minutes, and
argument `deg` should be of type `integer`. In the case of degrees,
minutes and seconds, both arguments `deg` and `min` must be of type
`integer` and argument `sec` should have a decimal point after the
number of whole seconds.

Negative coordinates i.e., S or W, rather than N or E, may be specified
by negative values of `deg`, `min`, or `sec`; only one, the first
non-zero value, of the three may be negative or an error will result.

The total value in degrees, minutes and seconds may not be greater than
`180˚`, while the minutes and seconds components (if present) must be
less than `60˚`. If latitude is represented (i.e., `latorlon` attribute
is `"lat"`), its maximum absolute value is `90˚`. Errors will be
reported if these limits are not observed.

`as_coord()` has S3 methods for both `"coord"` and
[`"waypoint"`](https://mark-eis.github.io/BitsnBobs/reference/waypoint.md)
objects and `numeric` values. Numeric values should have a decimal point
after the number of whole degrees in the case of decimal degrees, after
the number of whole minutes in the case of degrees and minutes, and
after the number of whole seconds in the case of degrees, minutes and
seconds.

There is also an S3 method for
[`as.numeric()`](https://rdrr.io/r/base/numeric.html) for objects of
class `"coord"`, returning numeric values as described above for
`as_coord()`.

## See also

Other coord:
[`waypoint()`](https://mark-eis.github.io/BitsnBobs/reference/waypoint.md)

## Examples

``` r
## Create "coord" objects

## Decimal degrees
(cdd <- coord(51.507765))
#>   51.507765°
coord(-0.127924)
#>   -0.127924°
coord(51.507765,,, "lat")
#>   51.507765° lat
coord(-0.127924,,, "lon")
#>   -0.127924° lon
coord(-53.104781,,, "lat")
#>  -53.104781° lat
coord(73.517283,,, "lon")
#>   73.517283° lon

## Degrees and (decimal) minutes
(cdm <- coord(51L, 30.4659))
#>  51°30.4659' (N/E)
coord(0L, -7.6754)
#>   0°07.6754' (W/S)
coord(51L, 30.4659,, "lat")
#>  51°30.4659' N
coord(0L, -7.6754,, "lon")
#>   0°07.6754' W
coord(-53L, 6.2869,, "lat")
#>  53°06.2869' S
coord(73L, 31.0370,, "lon")
#>  73°31.0370' E

## Degrees, minutes and (decimal) seconds
(cdms <- coord(51L, 30L, 27.95))
#>  51°30'27.95" (N/E)
coord(0L, -7L, 40.53)
#>   0°07'40.53" (W/S)
coord(51L, 30L, 27.95, "lat")
#>  51°30'27.95" N
coord(0L, -7L, 40.53, "lon")
#>   0°07'40.53" W
coord(-53L, 06L, 17.21, "lat")
#>  53°06'17.21" S
coord(73L, 31L, 02.22, "lon")
#>  73°31'02.22" E

## Convert between "coord" object formats

## To decimal degrees
cdd |> as_coord(.fmt = "decdeg")
#>   51.507765°
cdm |> as_coord(.fmt = "decdeg")
#>   51.507765°
cdms |> as_coord(.fmt = "decdeg")
#>   51.507764°

## To degrees and minutes
cdd |> as_coord(.fmt = "degmin")
#>  51°30.4659' (N/E)
cdm |> as_coord(.fmt = "degmin")
#>  51°30.4659' (N/E)
cdms |> as_coord(.fmt = "degmin")
#>  51°30.4658' (N/E)

## To degrees, minutes and seconds
cdd |> as_coord(.fmt = "degminsec")
#>  51°30'27.95" (N/E)
cdm |> as_coord(.fmt = "degminsec")
#>  51°30'27.95" (N/E)
cdms |> as_coord(.fmt = "degminsec")
#>  51°30'27.95" (N/E)


## Convert "coord" to numeric

## Decimal degrees
cdd |> as.numeric()
#> [1] 51.507765

## Degrees and minutes
cdm |> as.numeric()
#> [1] 5130.4659

## Degrees, minutes and seconds
cdms |> as.numeric()
#> [1] 513027.95


## Convert numeric to "coord" object

## Decimal degrees
as_coord(51.507765, .fmt = "decdeg")
#>   51.507765°
as_coord(-0.127924, .fmt = "decdeg")
#>   -0.127924°
as_coord(-53.104781, .fmt = "decdeg", .latorlon = "lat")
#>  -53.104781° lat
as_coord(73.517283, .fmt = "decdeg", .latorlon = "lon")
#>   73.517283° lon

## Degrees and minutes
as_coord(5130.4659, .fmt = "degmin")
#>  51°30.4659' (N/E)
as_coord(-07.6754, .fmt = "degmin")
#>   0°07.6754' (W/S)
as_coord(-5130.4659, .fmt = "degmin", .latorlon = "lat")
#>  51°30.4659' S
as_coord(7331.0370, .fmt = "degmin", .latorlon = "lon")
#>  73°31.0370' E

## Degrees, minutes and seconds
as_coord(513027.95, .fmt = "degminsec")
#>  51°30'27.95" (N/E)
as_coord(-0740.53, .fmt = "degminsec")
#>   0°07'40.53" (W/S)
as_coord(-530617.21, .fmt = "degminsec", .latorlon = "lat")
#>  53°06'17.21" S
as_coord(733102.22, .fmt = "degminsec", .latorlon = "lon")
#>  73°31'02.22" E

rm(cdd, cdm, cdms)
```
