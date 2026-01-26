# Design Effect for Cluster Sampling and Cluster Randomised Trials

The design effect is the ratio of the total number of subjects required
using cluster randomisation to the number required using individual
randomisation.

## Usage

``` r
design_effect(m, ri)
```

## Arguments

- m:

  integer representing cluster size.

- ri:

  intracluster (=intraclass) correlation coefficient.

## Value

Numeric value of design effect.

## Details

The design effect can be presented neatly in terms of the intracluster
correlation and the number in a single cluster: -

\$\$D = 1 + (m − 1)r\_{I}\$\$

If there is only one observation per cluster, `m = 1`, the design effect
is 1.0 and the two designs are the same. Otherwise, the larger the
intracluster correlation — that is, the more important the variation
between clusters is — the bigger the design effect and the more subjects
we will need to get the same power as a simply randomised study. Even a
small intracluster correlation will have an impact if the cluster size
is large.

## Note

*Description* and *Details* taken verbatim from the second reference.

## References

Kerry, S.M. & Bland, J.M., 1998. Sample size in cluster randomisation.
*Brit Med J* **316**: 5490.
[](https://doi.org/10.1136/bmj.316.7130.549)[doi:10.1136/bmj.316.7130.549](https://doi.org/10.1136/bmj.316.7130.549)
.

Kerry, S.M. & Bland, J.M., 1998. The intracluster correlation
coefficient in cluster randomisation. *Brit Med J* **316**: 1455-1460.
[](https://doi.org/10.1136/bmj.316.7142.1455)[doi:10.1136/bmj.316.7142.1455](https://doi.org/10.1136/bmj.316.7142.1455)
.

## See also

Other sample-size:
[`sample_size()`](https://mark-eis.github.io/BitsnBobs/reference/sample_size.md)

## Examples

``` r
## Example: x-ray guidelines study from second reference.
design_effect(m = 50L, ri = 0.019)
#> [1] 1.931
```
