# Area under disease progress curve

Calculates the area under disease progress curves.

## Usage

``` r
AUDPC(time, y, y_proportion = TRUE, type = "absolute")
```

## Arguments

- time:

  A vector object of time.

- y:

  A vector object of disease intensity.

- y_proportion:

  Logical. If disease intensity is proportion (TRUE) or
  percentage(FALSE).

- type:

  Set if is absolute or relative AUDPC. type = "absolute" is default.

## References

Madden, L. V., Hughes, G., and van den Bosch, F. 2007. The Study of
Plant Disease Epidemics. American Phytopathological Society, St. Paul,
MN.

## Author

Kaique dos S. Alves

## Examples

``` r
epi =  sim_logistic(N = 30, y0 = 0.01,dt = 5, r = 0.3, alpha = 0.5, n = 1)
AUDPC(time = epi$time, y = epi$y, y_proportion = TRUE)
#> [1] 14.69147
```
