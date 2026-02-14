# Changelog

## epifitter 0.3.0

CRAN release: 2021-06-14

#### New feature

- Including dataset `PowderyMildew` containing experimental data of
  disease progress curves of powdery mildew under different irrigation
  systems and soil moisture levels in organic tomato

- Included the option to define the maximum asymptote `K` when
  simulating disease progress curves using the function
  [`sim_monomolecular()`](https://alvesks.github.io/epifitter/reference/sim_monomolecular.md),[`sim_logistic()`](https://alvesks.github.io/epifitter/reference/sim_logistic.md),
  and
  [`sim_gompertz()`](https://alvesks.github.io/epifitter/reference/sim_gompertz.md)

------------------------------------------------------------------------

## epifitter 0.2.0

CRAN release: 2020-11-25

#### New feature

- Added a warning to the functions
  [`fit_nlin()`](https://alvesks.github.io/epifitter/reference/fit_nlin.md)
  and
  [`fit_nlin2()`](https://alvesks.github.io/epifitter/reference/fit_nlin2.md)
  for when the user insert disease intensity data out of the interval 0
  and 1.

- Added a `NEWS.md` file to track changes to the package.

#### Bug fix

fixed a problem with the
[`fit_lin()`](https://alvesks.github.io/epifitter/reference/fit_lin.md)
`$data` output.

------------------------------------------------------------------------

## epifitter 0.1.0

CRAN release: 2020-10-29

- First version: package assemble.
