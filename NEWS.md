# epifitter 0.3.0

### New feature
* Including dataset `PowderyMildew` containing experimental data of disease progress curves of powdery mildew under different irrigation systems and soil moisture levels in organic tomato

* Included the option to define the maximum asymptote `K` when simulating disease progress curves using the function `sim_monomolecular()`,`sim_logistic()`, and `sim_gompertz()`


--------------------------------------------------------------------------------

# epifitter 0.2.0

### New feature

* Added a warning to the functions `fit_nlin()` and `fit_nlin2()` for when the user insert disease intensity data out of the interval 0 and 1. 

* Added a `NEWS.md` file to track changes to the package.

### Bug fix

fixed a problem with the `fit_lin()` `$data` output.

--------------------------------------------------------------------------------

# epifitter 0.1.0

* First version: package assemble.
