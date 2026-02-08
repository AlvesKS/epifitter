# Simulate an epidemic using the logistic model

Simulate a stochastic epidemic curve using the logistic model.

## Usage

``` r
sim_logistic(N = 10,dt = 1, y0 = 0.01, r, K = 1, n,  alpha = 0.2)
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
sim_logistic(N = 30, y0 = 0.01,dt = 5, r = 0.3, K = 1, alpha = 0.5, n = 4)
#>    replicates time          y   random_y
#> 1           1    0 0.01000000 0.01000000
#> 2           1    5 0.04331353 0.05355248
#> 3           1   10 0.16868040 0.15624711
#> 4           1   15 0.47626911 0.41316690
#> 5           1   20 0.80297496 0.90921348
#> 6           1   25 0.94809157 0.94281142
#> 7           1   30 0.98793024 0.98685972
#> 8           2    0 0.01000000 0.01000000
#> 9           2    5 0.04331353 0.05807907
#> 10          2   10 0.16868040 0.16352253
#> 11          2   15 0.47626911 0.47157544
#> 12          2   20 0.80297496 0.74905351
#> 13          2   25 0.94809157 0.94011226
#> 14          2   30 0.98793024 0.98828892
#> 15          3    0 0.01000000 0.01000000
#> 16          3    5 0.04331353 0.05432546
#> 17          3   10 0.16868040 0.06222023
#> 18          3   15 0.47626911 0.51450253
#> 19          3   20 0.80297496 0.68143704
#> 20          3   25 0.94809157 0.94068546
#> 21          3   30 0.98793024 0.98478062
#> 22          4    0 0.01000000 0.01000000
#> 23          4    5 0.04331353 0.04213470
#> 24          4   10 0.16868040 0.03445765
#> 25          4   15 0.47626911 0.62301073
#> 26          4   20 0.80297496 0.67127050
#> 27          4   25 0.94809157 0.93668549
#> 28          4   30 0.98793024 0.98127708
```
