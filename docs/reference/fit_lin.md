# Fits epidemic models using data linearization

Fits epidemic models (Exponential, Monomolecular, Logistic and Gompertz)
to data using data linearization

## Usage

``` r
fit_lin(time,y)
```

## Arguments

- time:

  Numeric vector which refers to the time steps in the epidemics

- y:

  Numeric vector which refers to the disease intensity

## Author

Kaique dos S. Alves

## Examples

``` r
set.seed(1)
epi1 <- sim_logistic(N = 30,
                     y0 = 0.01,
                     dt = 5,
                     r = 0.3,
                     alpha = 0.2,
                     n = 4)
data = data.frame(time =  epi1[,2], y = epi1[,4])
fit_lin( time = data$time, y =  data$y)
#> Results of fitting population models 
#> 
#> Stats:
#>                  CCC r_squared    RSE
#> Exponential   0.9326    0.8737 0.5983
#> Gompertz      0.9786    0.9581 0.4304
#> Logistic      0.9982    0.9964 0.1848
#> Monomolecular 0.9318    0.8723 0.5743
#> 
#>  Infection rate:
#>                Estimate   Std.error     Lower     Upper        rho
#> Exponential   0.1516750 0.011307165 0.1284328 0.1284328 0.02527916
#> Gompertz      0.1984260 0.008134087 0.1817061 0.1817061 0.04960650
#> Logistic      0.2963172 0.003491710 0.2891399 0.2891399 0.04938621
#> Monomolecular 0.1446423 0.010852569 0.1223345 0.1223345 0.07232113
#> 
#>  Initial inoculum:
#>                    Estimate Linearized     lin.SE         Lower        Upper
#> Exponential    0.0231954412 -3.7637995 0.20384281  0.0152556644  0.035267457
#> Gompertz       0.0002387652 -2.1210668 0.14663934  0.0000127004  0.002091927
#> Logistic       0.0108173681 -4.5157260 0.06294769  0.0095169644  0.012293254
#> Monomolecular -1.1210822598 -0.7519265 0.19564748 -2.1711224375 -0.418737384
```
