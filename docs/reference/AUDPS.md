# Area under disease progress stairs

Calculates the area under disease progress stairs.

## Usage

``` r
AUDPS(time, y, y_proportion = TRUE, type = "absolute")
```

## Arguments

- time:

  A vector object of time.

- y:

  A vector object of disease intensity.

- y_proportion:

  Logical. If disease intensity is proportion (TRUE) or
  percentage(FALSE)

- type:

  Set if is absolute or relative AUDPC. type = "absolute" is default.

## References

Simko, I., and Piepho, H.-P. 2012. The area under the disease progress
stairs: Calculation, advantage, and application. Phytopathology 102:381-
389.

## Author

Kaique dos S. Alves

## Examples

``` r
epi =  sim_logistic(N = 30, y0 = 0.01,dt = 5, r = 0.3, alpha = 0.5, n = 1)
AUDPS(time = epi$time, y = epi$y, y_proportion = TRUE)
#> [1] 17.1863
```
