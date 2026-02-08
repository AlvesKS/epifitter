# Simulate an epidemic using the Exponential model

Simulate a stochastic epidemic curve using the Exponential model.

## Usage

``` r
sim_exponential(N = 10,dt = 1, y0 = 0.01, r, n,  alpha = 0.2)
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
sim_exponential(N = 30, y0 = 0.01,dt = 5, r = 0.1, alpha = 0.5, n = 4)
#>    replicates time          y   random_y
#> 1           1    0 0.01000000 0.01000000
#> 2           1    5 0.01648856 0.03628817
#> 3           1   10 0.02718643 0.02665303
#> 4           1   15 0.04482448 0.06028308
#> 5           1   20 0.07390504 0.07493979
#> 6           1   25 0.12185136 0.07656693
#> 7           1   30 0.20090235 0.21986676
#> 8           2    0 0.01000000 0.01000000
#> 9           2    5 0.01648856 0.02857101
#> 10          2   10 0.02718643 0.02926963
#> 11          2   15 0.04482448 0.09351757
#> 12          2   20 0.07390504 0.09147631
#> 13          2   25 0.12185136 0.07859739
#> 14          2   30 0.20090235 0.26225053
#> 15          3    0 0.01000000 0.01000000
#> 16          3    5 0.01648856 0.01000000
#> 17          3   10 0.02718643 0.03114812
#> 18          3   15 0.04482448 0.03488931
#> 19          3   20 0.07390504 0.07394588
#> 20          3   25 0.12185136 0.12638065
#> 21          3   30 0.20090235 0.14168428
#> 22          4    0 0.01000000 0.01000000
#> 23          4    5 0.01648856 0.01537411
#> 24          4   10 0.02718643 0.04320042
#> 25          4   15 0.04482448 0.01067793
#> 26          4   20 0.07390504 0.09585284
#> 27          4   25 0.12185136 0.14213659
#> 28          4   30 0.20090235 0.30769198
```
