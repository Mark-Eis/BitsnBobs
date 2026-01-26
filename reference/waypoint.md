# Waypoint

A robust representation of a geographic or GPS waypoint comprising
latitude and langitude values implemented as
[`"coord"`](https://mark-eis.github.io/BitsnBobs/reference/coord.md)
objects.

## Usage

``` r
waypoint(..., .fmt = c("decdeg", "degmin", "degminsec"))
```

## Arguments

- ...:

  two `numeric` values or two `"coord"` objects.

- .fmt:

  `character` string indicating the desired format; must be one of
  `"decdeg"` (default), `"degmin"` or `"degminsec"`.

## Value

An object of `class` `"waypoint"` comprising a named list of two
`"coord"` objects representing the latitude and longitude values of a
waypoint.

## Details

Waypoints may be defined using (i) decimal degrees, (ii) degrees and
minutes, or (iii) degrees, minutes and seconds. They may easily be
converted between formats using
[`as_coord()`](https://mark-eis.github.io/BitsnBobs/reference/coord.md).

## See also

Other coord:
[`coord()`](https://mark-eis.github.io/BitsnBobs/reference/coord.md)

## Examples

``` r
## Create "waypoint" objects

## Decimal degrees
(wp_dd <- waypoint(coord(51.507765), coord(-0.127924)))
#>   51.507765° lat    -0.127924° lon
waypoint(as_coord(51.507765), as_coord(-0.127924))
#>   51.507765° lat    -0.127924° lon
waypoint(51.507765, -0.127924)
#>   51.507765° lat    -0.127924° lon

## Degrees amd minutes
(wp_dm <- waypoint(coord(51L, 30.4659), coord(0L, -07.6754)))
#>  51°30.4659' N    0°07.6754' W
waypoint(as_coord(5130.4659, .fmt = "degmin"), as_coord(-007.6754, .fmt = "degmin"))
#>  51°30.4659' N    0°07.6754' W
waypoint(5130.4659, -007.6754, .fmt = "degmin")
#>  51°30.4659' N    0°07.6754' W

## Degrees, minutes and seconds
(wp_dms <- waypoint(coord(51L, 30L, 27.95), coord(0L, -07L, 40.53)))
#>  51°30'27.95" N    0°07'40.53" W
waypoint(as_coord(513027.95, .fmt = "degminsec"), as_coord(-00740.53, .fmt = "degminsec"))
#>  51°30'27.95" N    0°07'40.53" W
waypoint(513027.95, -00740.53, .fmt = "degminsec")
#>  51°30'27.95" N    0°07'40.53" W

## Convert between "waypoint" object formats

## To decimal degrees
wp_dd |> as_coord(.fmt = "decdeg")
#>   51.507765° lat    -0.127924° lon
wp_dm |> as_coord(.fmt = "decdeg")
#>   51.507765° lat    -0.127923° lon
wp_dms |> as_coord(.fmt = "decdeg")
#>   51.507764° lat    -0.127925° lon

## To degrees and minutes
wp_dd |> as_coord(.fmt = "degmin")
#>  51°30.4659' N    0°07.6754' W
wp_dm |> as_coord(.fmt = "degmin")
#>  51°30.4659' N    0°07.6754' W
wp_dms |> as_coord(.fmt = "degmin")
#>  51°30.4658' N    0°07.6755' W

## To degrees, minutes and seconds
wp_dd |> as_coord(.fmt = "degminsec")
#>  51°30'27.95" N    0°07'40.53" W
wp_dm |> as_coord(.fmt = "degminsec")
#>  51°30'27.95" N    0°07'40.52" W
wp_dms |> as_coord(.fmt = "degminsec")
#>  51°30'27.95" N    0°07'40.53" W

rm(wp_dd, wp_dm, wp_dms)
```
