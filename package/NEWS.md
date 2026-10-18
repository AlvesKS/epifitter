# epifitter 1.0.1

### New feature
* Added optional `multistart = TRUE` to nonlinear and grouped fitting, comparing converged attempts by residual sum of squares with fixed weights within each stage. The default preserves the first-converged strategy. Printed nonlinear results explain the selected strategy, and bootstrap refits inherit it.

### Bug fix
* The manual citation takes its package version from metadata instead of a hard-coded release number.
* Linearized fitting reuses each regression, and bootstrap intervals refit only the requested model while sharing the nonlinear fitting rules.
* Bootstrap bands require at least two successful finite predictions per model, with model-specific warnings for failed fits.
* `AUDPC_2_points()` uses a numerically stable logistic integral for nearly equal observations and handles constant curves explicitly.
* Simulators now use analytical solutions, avoiding numerical overshoot near the asymptote and removing the runtime dependency on deSolve.
* Grouped fitting uses native group keys, preserving missing labels and column types without merging distinct strata or overwriting a `strata` column.
* Wild bootstrap now changes the sign of each centered residual without moving residuals between observations.
* Bootstrap confidence intervals now keep numeric weights aligned when assessment times are sorted.
* Nonlinear fitting now rejects unconverged optimizer results and tries the remaining starting values before reporting failure.
* Fixed monomolecular fits returning `NA` when initial disease intensity reaches its lower bound in `fit_nlin()` and `fit_nlin2()` (#6).
* `fit_nlin2()` allows noisy observations above the fitted asymptote instead of forcing `K` to exceed the largest observation.
* Fixed `AUDPC()` trapezoid indexing so results match the documented `agricolae::audpc()` calculation.
* `AUDPC()` and `AUDPS()` now warn when repeated assessment times are aggregated before area calculation.

--------------------------------------------------------------------------------

# epifitter 1.0.0

### Highlights
* Modernized the fitting workflow with shared input validation, more informative error messages, and improved grouped-model ergonomics.
* Refreshed the documentation site with updated articles, improved accessibility, dark mode support, and a clearer visual style for figures.
* Added a formal package citation, a vignette built around the bundled `PowderyMildew` dataset, and expanded regression tests for area summaries and model fitting.
* Corrected area-under-the-curve handling for repeated assessment times and strengthened defaults for nonlinear fitting workflows.
* Added optional confidence bands to `plot_fit()` with residual and wild bootstrap interval methods.
* Strengthened simulation input validation and corrected the logistic simulation equation when `K` is lower than 1.
* Added optional weighted nonlinear least squares to `fit_nlin()` and `fit_nlin2()`, including custom numeric weights, custom weight functions, and fitted-value weighting methods (`"binomial"`, `"mean"`, `"cv"`, and `"power"`) for proportional disease intensity data.

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
