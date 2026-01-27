# Expand Expression as a Power Sequence

Create a power sequence from an expression, `base_expr`, and the maximum
power number, `n`.

## Usage

``` r
power_seq(base_expr, n, type = c("simple", "evaluate", "replicate"))
```

## Arguments

- base_expr:

  a bare (quoted) expression, see examples.

- n:

  a non-negative integer or number coercible into a positive integer.

- type:

  a character string, (partially) matching one of `"simple"`,
  `"evaluate"` or `"replicate"`; default `"simple"`.

## Value

A `"call"` object i.e., a captured function call, possibly nested `n`
times.

## Details

The power sequence is returned as a `"call"` object, which may be
evaluated using
[`eval_tidy`](https://rlang.r-lib.org/reference/eval_tidy.html) in
package [rlang](https://rlang.r-lib.org/reference/rlang-package.html),
see examples. By default, `power_seq()` returns a simple `call` object
containing just one instance of both `base_expr` and `n`.

Alternatively, if `type` is `"evaluate"`, `power_seq()` evaluates
`base_expr` and assigns the result to a constant nested `n` times within
the `call` object to be returned. This results in `base_expr` being
evaluated on execution of `power_seq()` rather than on evaluation of the
returned call, which may be preferable for more complex expressions.

Finally, if `type` is `"replicate"`, `power_seq()` captures the
`base_expr` argument and its environment as a
[`quosure`](https://rlang.r-lib.org/reference/quosure-tools.html) to be
replicated `n` times within the returned nested `call` object. This
results in `base_expr` being evaluated `n` times on evaluation of the
returned call; this multiple evaluation may be acceptable in simple
cases but might be slow and inefficient for complex expressions.

## See also

[`call`](https://rdrr.io/r/base/call.html),
[`eval_tidy`](https://rlang.r-lib.org/reference/eval_tidy.html) and
[`quosure`](https://rlang.r-lib.org/reference/quosure-tools.html).

Other powerseq:
[`formul_pwrseq()`](https://mark-eis.github.io/BitsnBobs/reference/formul_pwrseq.md)

## Examples

``` r
(pseq <- power_seq(a + b, 3))
#> sum((~a + b)^seq_len(~3))
## pseq is a "call" object
typeof(pseq)
#> [1] "language"
class(pseq)
#> [1] "call"
mode(pseq)
#> [1] "call"
is.call(pseq)
#> [1] TRUE
as.list(pseq)
#> [[1]]
#> sum
#> 
#> [[2]]
#> (~a + b)^seq_len(~3)
#> 
is.call(pseq[[2]])
#> [1] TRUE
as.list(pseq[[2]])
#> [[1]]
#> `^`
#> 
#> [[2]]
#> <quosure>
#> expr: ^a + b
#> env:  0x55db2b9c49b8
#> 
#> [[3]]
#> seq_len(~3)
#> 
is.call(pseq[[2]][[3]])
#> [1] TRUE
as.list(pseq[[2]][[3]])
#> [[1]]
#> seq_len
#> 
#> [[2]]
#> <quosure>
#> expr: ^3
#> env:  empty
#> 

## View the abstract syntax tree - requires {lobstr} package
if (!requireNamespace("lobstr", quietly = TRUE)) 
  warning("package 'lobstr' must be installed")
try(lobstr::ast(!!pseq))
#> █─sum 
#> └─█─`^` 
#>   ├─█─`+` 
#>   │ ├─a 
#>   │ └─b 
#>   └─█─seq_len 
#>     └─3 

(pseq2 <- power_seq(log(x), 5))
#> sum((~log(x))^seq_len(~5))

x <- 3
eval_tidy(pseq2)  ## Uses x from the global environment
#> [1] 6.688633

x <- 5
eval_tidy(pseq2)
#> [1] 25.87694

rm(x)
try(eval_tidy(pseq2))
#> Error : object 'x' not found

foo <- function() {
  x <- 10
  power_seq(log(x), 5)
}

pseq2 <- foo()
pseq2                 ## Expression looks just the same but …
#> sum((~log(x))^seq_len(~5))

x <- 3
eval_tidy(pseq2)  ## Consistently uses x from the environment of foo()
#> [1] 112.6486

x <- 5
eval_tidy(pseq2)
#> [1] 112.6486

rm(x)
eval_tidy(pseq2)
#> [1] 112.6486

## Wrapper for log() reporting its execution using marker()
log <- function(...) {
  marker(msg = "in BitsnBobs Help")
  base::log(...)
}

## Compare the three options for type 
## log() invoked just once, on execution of power_seq() with type = "evaluate"
(expr_ls <- c("simple", "evaluate", "replicate") |> setNames(nm = _) |>
    lapply(\(x) power_seq(log(3), 5, x)))
#> log running in BitsnBobs Help 
#> $simple
#> sum((~log(3))^seq_len(~5))
#> 
#> $evaluate
#> 1.09861228866811 + 1.09861228866811^2L + 1.09861228866811^3L + 
#>     1.09861228866811^4L + 1.09861228866811^5L
#> 
#> $replicate
#> (~log(3)) + (~log(3))^2L + (~log(3))^3L + (~log(3))^4L + (~log(3))^5L
#> 

## log() invoked once on evaluation of expression from power_seq() with type = "simple" and
## five times on evaluation of expression from power_seq() with type = "replicate"
(res_ls <- expr_ls |> lapply(eval_tidy))
#> log running in BitsnBobs Help 
#> log running in BitsnBobs Help 
#> log running in BitsnBobs Help 
#> log running in BitsnBobs Help 
#> log running in BitsnBobs Help 
#> log running in BitsnBobs Help 
#> $simple
#> [1] 6.688633
#> 
#> $evaluate
#> [1] 6.688633
#> 
#> $replicate
#> [1] 6.688633
#> 

## All three types evaluate identically: -
all(
  identical(res_ls[[1]], res_ls[[2]]),
  identical(res_ls[[1]], res_ls[[3]]),
  identical(res_ls[[2]], res_ls[[3]])
)
#> [1] TRUE

## Compare the three abstract syntax trees
try(expr_ls |> lapply(\(x) lobstr::ast(!!x)))
#> $simple
#> █─sum 
#> └─█─`^` 
#>   ├─█─log 
#>   │ └─3 
#>   └─█─seq_len 
#>     └─5 
#> 
#> $evaluate
#> █─`+` 
#> ├─█─`+` 
#> │ ├─█─`+` 
#> │ │ ├─█─`+` 
#> │ │ │ ├─1.09861228866811 
#> │ │ │ └─█─`^` 
#> │ │ │   ├─1.09861228866811 
#> │ │ │   └─2L 
#> │ │ └─█─`^` 
#> │ │   ├─1.09861228866811 
#> │ │   └─3L 
#> │ └─█─`^` 
#> │   ├─1.09861228866811 
#> │   └─4L 
#> └─█─`^` 
#>   ├─1.09861228866811 
#>   └─5L 
#> 
#> $replicate
#> █─`+` 
#> ├─█─`+` 
#> │ ├─█─`+` 
#> │ │ ├─█─`+` 
#> │ │ │ ├─█─log 
#> │ │ │ │ └─3 
#> │ │ │ └─█─`^` 
#> │ │ │   ├─█─log 
#> │ │ │   │ └─3 
#> │ │ │   └─2L 
#> │ │ └─█─`^` 
#> │ │   ├─█─log 
#> │ │   │ └─3 
#> │ │   └─3L 
#> │ └─█─`^` 
#> │   ├─█─log 
#> │   │ └─3 
#> │   └─4L 
#> └─█─`^` 
#>   ├─█─log 
#>   │ └─3 
#>   └─5L 
#> 

rm(expr_ls, foo, log, pseq, pseq2, res_ls)
```
