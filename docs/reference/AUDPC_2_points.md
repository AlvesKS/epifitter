# Estimate disease progress curve from only two points

Estimates area under the disease progress curve AUDPC from two
qualifications only, see Jeger and Viljanen-Rollinson (2001) for more.

## Usage

``` r
AUDPC_2_points(time, y0, yT)
```

## Arguments

- time:

  Number of days from first disease assessment to last disease
  assessment.

- y0:

  A numeric value representing the disease intensity at the first
  intensity measurement. Must be greater than zero and less than one.

- yT:

  A numeric value representing the disease intensity at the last
  intensity measurement. Must be greater than `y0` and less than one.

## References

Jeger, M. J., & Viljanen-Rollinson, S. L. H. (2001). The use of the area
under the disease-progress curve (AUDPC) to assess quantitative disease
resistance in crop cultivars. \*Theoretical and Applied Genetics\*, 102,
32-40. DOI:
[10.1007/s001220051615](https://doi.org/10.1007/s001220051615).

## Author

Adam H. Sparks, <adamhsparks@gmail.com>

## Examples

``` r
epi <- sim_logistic(N = 30, y0 = 0.01, dt = 5, r = 0.3, alpha = 0.5, n = 1)

# compare with use values from `epi`, first and last only
# time
epi$time[7]
#> [1] 30

# y0
epi$y[1]
#> [1] 0.01

# yT
epi$y[7]
#> [1] 0.9879302

AUDPC_2_points(time = epi$time[7], y0 = epi$y[1], yT = epi$y[7])
#> [1] 14.68996

# compare with traitional AUDPC trapezoidal method
AUDPC(time = epi$time, y = epi$y, y_proportion = TRUE)
#> [1] 14.69147
```
