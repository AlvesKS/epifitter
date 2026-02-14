# Fits epidemic models using nonlinear aproach. This function also estimates the maximum disease intensity parameter K

Fits epidemic models (Exponential, Monomolecular, Logistic and Gompertz)
using nonlinear approach for estimate parameters. This function also
estimates the maximum disease intensity parameter K.

## Usage

``` r
fit_nlin2(time,
  y,
  starting_par = list(y0 = 0.01, r = 0.03, K =  0.8),
  maxiter = 50)
```

## Arguments

- time:

  Numeric vector which refers to the time steps in the epidemics.

- y:

  Numeric vector which refers to the disease intensity.

- starting_par:

  starting value for initial inoculun (y0) and apparent infection rate
  (r), and maximum disease intensity (K). Please informe in that
  especific order

- maxiter:

  Maximun number of iterations.

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
fit_nlin2(time = data$time,
          y =  data$y,
          starting_par = list(y0 = 0.01, r = 0.03, K = 1),
          maxiter = 1024)
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
#> Monomolecular 0.8960    0.8361 0.1797
#> Gompertz      0.9950    0.9909 0.0438
#> Logistic      0.9923    0.9860 0.0539
#> 
#>  Infection rate:
#>                Estimate  Std.error      Lower      Upper        rho
#> Monomolecular 0.0682291 0.02690929 0.01280837 0.01280837 0.03411455
#> Gompertz      0.2630455 0.02071581 0.22038054 0.22038054 0.06576138
#> Logistic      0.3661332 0.03408759 0.29592845 0.29592845 0.06102219
#> 
#>  Initial inoculum:
#>                    Estimate    Std.error         Lower        Upper
#> Monomolecular -1.487701e-01 8.495704e-02 -3.237424e-01 2.620220e-02
#> Gompertz       1.017021e-12 7.351869e-12 -1.412444e-11 1.615848e-11
#> Logistic       5.304329e-03 2.520782e-03  1.126822e-04 1.049598e-02
#> 
#>  Maximum disease intensity:
#>               Estimate  Std.error     Lower    Upper
#> Monomolecular        1 0.19490764 0.5985802 1.401420
#> Gompertz             1 0.01936234 0.9601225 1.039877
#> Logistic             1 0.02080931 0.9571424 1.042858
```
