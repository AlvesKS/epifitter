# Simulate an epidemic using the Gompertz model

Simulate a stochastic epidemic curve using the Gompertz model.

## Usage

``` r
sim_gompertz(N = 10,dt = 1, y0 = 0.01, r, K = 1, n,  alpha = 0.2)
```

## Arguments

- N:

  Total time course of the epidemic

- dt:

  Time step

- y0:

  Initial inoculum or initial disease intensity

- r:

  Infection rate

- K:

  Maximum asymptote

- n:

  Number or replicates or sample size for each time step

- alpha:

  Variation parameter. stands for the variation for the replicates for
  each time step. The standard deviation is calculated as sd = alpha \*
  y \* (1 - y), being y the disease intensity for each time step.

## Value

- rep:

  Replicates

- time:

  Time after epidemic start

- y:

  Disease intensity

- random_y:

  Disease intensity after applying the random `alpha` error

## Examples

``` r
sim_gompertz(N = 30, y0 = 0.01,dt = 5, r = 0.3, K = 1, alpha = 0.5, n = 4)
#>    replicates time         y   random_y
#> 1           1    0 0.0100000 0.01183159
#> 2           1    5 0.3578962 0.38858672
#> 3           1   10 0.7951152 0.75092507
#> 4           1   15 0.9501253 0.97874406
#> 5           1   20 0.9886489 0.99516008
#> 6           1   25 0.9974559 0.99834437
#> 7           1   30 0.9994315 0.99988229
#> 8           2    0 0.0100000 0.01276451
#> 9           2    5 0.3578962 0.21121160
#> 10          2   10 0.7951152 0.74842075
#> 11          2   15 0.9501253 0.92110987
#> 12          2   20 0.9886489 0.98599263
#> 13          2   25 0.9974559 0.99666882
#> 14          2   30 0.9994315 0.99944343
#> 15          3    0 0.0100000 0.01000000
#> 16          3    5 0.3578962 0.37605422
#> 17          3   10 0.7951152 0.74179701
#> 18          3   15 0.9501253 0.99199872
#> 19          3   20 0.9886489 0.99267046
#> 20          3   25 0.9974559 0.99861076
#> 21          3   30 0.9994315 0.99954061
#> 22          4    0 0.0100000 0.01832677
#> 23          4    5 0.3578962 0.28484801
#> 24          4   10 0.7951152 0.75751265
#> 25          4   15 0.9501253 0.98406125
#> 26          4   20 0.9886489 0.98499780
#> 27          4   25 0.9974559 0.99719281
#> 28          4   30 0.9994315 0.99931986
```
