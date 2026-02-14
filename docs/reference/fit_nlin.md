# Fits epidemic models using nonlinear aproach

Fits epidemic models (Exponential, Monomolecular, Logistic and Gompertz)
using nonlinear approach for estimate parameters.

## Usage

``` r
fit_nlin(time,
  y,
  starting_par = list(y0 = 0.01, r = 0.03),
  maxiter = 50)
```

## Arguments

- time:

  Numeric vector which refers to the time steps in the epidemics

- y:

  Numeric vector which refers to the disease intensity

- starting_par:

  Starting value for initial inoculun (y0) and apparent infection rate
  (r). Please informe in that especific order

- maxiter:

  Maximun number of iterations

## Author

Kaique dos S. Alves

## Examples

``` r
set.seed(1)
epi1 <- sim_logistic(N = 30,
                     y0 = 0.01,
                     dt = 5,
                     r = 0.3,
                     alpha = 0.5,
                     n = 4)
data = data.frame(time =  epi1[,2], y = epi1[,4])
fit_nlin(time = data$time, y =  data$y, starting_par = list(y0 = 0.001, r = 0.03), maxiter = 1024)
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Results of fitting population models 
#> 
#> Stats:
#>                  CCC r_squared    RSE
#> Monomolecular 0.8960    0.8361 0.1762
#> Exponential   0.8795    0.8131 0.1880
#> Gompertz      0.9950    0.9909 0.0429
#> Logistic      0.9923    0.9860 0.0529
#> 
#>  Infection rate:
#>                 Estimate   Std.error      Lower      Upper        rho
#> Monomolecular 0.06822917 0.008441786 0.05087684 0.05087684 0.03411459
#> Exponential   0.06438233 0.008534818 0.04683876 0.04683876 0.01073039
#> Gompertz      0.26304504 0.015218902 0.23176214 0.23176214 0.06576126
#> Logistic      0.36613264 0.028677439 0.30718532 0.30718532 0.06102211
#> 
#>  Initial inoculum:
#>                    Estimate    Std.error         Lower        Upper
#> Monomolecular -1.487705e-01 7.778021e-02 -3.086500e-01 1.110901e-02
#> Exponential    1.658502e-01 3.643013e-02  9.096701e-02 2.407334e-01
#> Gompertz       1.017193e-12 5.947383e-12 -1.120783e-11 1.324221e-11
#> Logistic       5.304364e-03 2.219734e-03  7.416344e-04 9.867093e-03
```
