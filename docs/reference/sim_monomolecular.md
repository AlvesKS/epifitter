# Simulate an epidemic using the Monomolecular model

Simulate a stochastic epidemic curve using the Monomolecular model.

## Usage

``` r
sim_monomolecular(N = 10,dt = 1, y0 = 0.01, r,K = 1, n,  alpha = 0.2)
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
sim_monomolecular(N = 30, y0 = 0.01,dt = 5, r = 0.3, K = 1, alpha = 0.5, n = 4)
#>    replicates time         y   random_y
#> 1           1    0 0.0100000 0.02033147
#> 2           1    5 0.7791000 0.78059692
#> 3           1   10 0.9507122 0.92057510
#> 4           1   15 0.9890023 0.98008002
#> 5           1   20 0.9975461 0.99809711
#> 6           1   25 0.9994522 0.99944709
#> 7           1   30 0.9998774 0.99985785
#> 8           2    0 0.0100000 0.01000000
#> 9           2    5 0.7791000 0.65110167
#> 10          2   10 0.9507122 0.92552122
#> 11          2   15 0.9890023 0.99444082
#> 12          2   20 0.9975461 0.99678572
#> 13          2   25 0.9994522 0.99907317
#> 14          2   30 0.9998774 0.99999197
#> 15          3    0 0.0100000 0.01210425
#> 16          3    5 0.7791000 0.75856404
#> 17          3   10 0.9507122 0.97551166
#> 18          3   15 0.9890023 0.99382298
#> 19          3   20 0.9975461 0.99678820
#> 20          3   25 0.9994522 0.99900000
#> 21          3   30 0.9998774 0.99986172
#> 22          4    0 0.0100000 0.01000000
#> 23          4    5 0.7791000 0.76667419
#> 24          4   10 0.9507122 0.95557465
#> 25          4   15 0.9890023 0.99900000
#> 26          4   20 0.9975461 0.99767560
#> 27          4   25 0.9994522 0.99957728
#> 28          4   30 0.9998774 0.99987263
```
