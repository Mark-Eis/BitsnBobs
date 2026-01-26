# S3 Method Debugging Functions

Functions intended for use in interactive mode from within the
[`browser()`](https://rdrr.io/r/base/browser.html).

## Usage

``` r
ls_all(all.names = TRUE, env = parent.frame(), ...)

s3mag7(env = parent.frame())

browse(fn, ...)
```

## Arguments

- all.names:

  a logical value. If `TRUE`, all object names are returned. If `FALSE`,
  names which begin with a `.` are omitted.

- env:

  an `environment` to use in listing the available objects, equivalent
  to the `name` argument of [`ls()`](https://rdrr.io/r/base/ls.html);
  default [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html)
  i.e., the `environment` in which the function was called.

- ...:

  additional arguments passed to
  [`ls()`](https://rdrr.io/r/base/ls.html) or `fn()`.

- fn:

  a `function` to be called in “browser mode”.

## Value

A [`character vector`](https://rdrr.io/r/base/character.html) for
`ls_all()`; for `s3mag7()`, a named
[`list`](https://rdrr.io/r/base/list.html) containing the following
elements: -

- `.Class`:

  .Class is a
  [`character vector`](https://rdrr.io/r/base/character.html) of
  [`classes`](https://rdrr.io/r/base/class.html) used to find the next
  [`method`](https://rdrr.io/r/utils/methods.html).
  [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) adds an
  attribute `"previous"` to .Class giving the .Class last used for
  dispatch, and shifts .Class along to that used for dispatch.

- `.Generic`:

  A length-one
  [`character vector`](https://rdrr.io/r/base/character.html) naming the
  generic function for the current
  [`method`](https://rdrr.io/r/utils/methods.html).

- `.GenericCallEnv`:

  The environment of the call to be generic.

- `.GenericDefEnv`:

  The environment defining the generic, used to find
  [`methods`](https://rdrr.io/r/utils/methods.html) registered for the
  generic.

- `.Group`:

  The generic [`group`](https://rdrr.io/r/base/groupGeneric.html) to
  which the [`method`](https://rdrr.io/r/utils/methods.html) belongs, if
  applicable.

- `.Method`:

  A character vector (normally of length one) naming the
  [`method`](https://rdrr.io/r/utils/methods.html) function. (For
  functions in the generic group
  [`Ops`](https://rdrr.io/r/base/groupGeneric.html), it is of length
  two.)

- `"object"`:

  i.e., the `"object"` comprising the first argument of the call to
  `.Generic`.

- `.class2("object")`:

  The exact full
  [`character vector`](https://rdrr.io/r/base/character.html) of the
  classes of `"object"` used by
  [`UseMethod()`](https://rdrr.io/r/base/UseMethod.html).

## Details

`ls_all()` returns a
[`character vector`](https://rdrr.io/r/base/character.html) giving the
names of all objects in its caller's
[`environment`](https://rdrr.io/r/base/environment.html) including any
that begin with a ‘⁠.⁠’, and is convenient shorthand for
`ls(.all.names = TRUE)`, for instance when used from within the
[`browser()`](https://rdrr.io/r/base/browser.html) in interactive mode.

Intended for use while debugging an S3
[`method`](https://rdrr.io/r/utils/methods.html) in interactive mode
with the [`browser()`](https://rdrr.io/r/base/browser.html), `s3mag7()`
returns a named list of seven ‘special’ objects in the S3 method
dispatch `environment`, see the **Technical Details** section of
[`UseMethod`](https://rdrr.io/r/base/UseMethod.html).

`browse()` calls a specified function `fn` in "browser" mode with
suitable arguments provided in `...`. The
[base](https://rdrr.io/r/base/base-package.html) function
[`debug`](https://rdrr.io/r/base/debug.html) is generally preferable.

## See also

[`browser()`](https://rdrr.io/r/base/browser.html),
[`class`](https://rdrr.io/r/base/class.html),
[`debug`](https://rdrr.io/r/base/debug.html),
[`environment`](https://rdrr.io/r/base/environment.html),
[`methods()`](https://rdrr.io/r/utils/methods.html),
[`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) and
[`UseMethod`](https://rdrr.io/r/base/UseMethod.html)

Other methods:
[`S3Gen_Meth`](https://mark-eis.github.io/BitsnBobs/reference/S3Gen_Meth.md),
[`method_info()`](https://mark-eis.github.io/BitsnBobs/reference/method_info.md)

## Examples

``` r
fn <- function() {
    m <- "Mimi"
    p <- "Poley"
    .b <- "Blossom"
    ls_all()
}

fn()
#> ls(all.names = TRUE) : -
#>  [1] ".b" "m"  "p" 
#> 

rm(fn)

## To run this in browser() interactive mode from R Console, select lines between
##   "## Not run:" and "## End(Not run)" and hit [shift][enter]

if (FALSE) { # \dontrun{

## Two-by-two table for diagnostic test comparison
(twobytwo <- matrix(
    c(31, 12, 4, 58),
    nrow = 2, 
    dimnames = rep(list(c("+ve", "-ve")), 2) |>
        setNames(c("Test1", "Test2"))
))

browse(print_all, cohens_kappa(twobytwo))
s
where
ls_all()
s
where
ls_all()
s
where
ls_all()
s
where
ls_all()
s3mag7()
f

rm(twobytwo)
} # }
```
