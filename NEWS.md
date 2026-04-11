# epifitter 1.0.0

### Highlights
* Modernized the fitting workflow with shared input validation, more informative error messages, and improved grouped-model ergonomics.
* Refreshed the documentation site with updated articles, improved accessibility, dark mode support, and a clearer visual style for figures.
* Added a formal package citation, a vignette built around the bundled `PowderyMildew` dataset, and expanded regression tests for area summaries and model fitting.
* Corrected area-under-the-curve handling for repeated assessment times and strengthened defaults for nonlinear fitting workflows.

--------------------------------------------------------------------------------

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
