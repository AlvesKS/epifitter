# *epifitter* <img src="man/figures/logo.png" width="150" align="right"/>

[![CRAN](https://www.r-pkg.org/badges/version/epifitter)](https://CRAN.R-project.org/package=epifitter) [![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/epifitter)](https://CRAN.R-project.org/package=epifitter)

`epifitter` provides tools for the **visualization, description, and comparison of plant disease progress curves** (DPCs). A DPC describes how disease intensity changes over time during an epidemic. By fitting classic population dynamics models such as logistic, monomolecular, Gompertz, and exponential curves, users can compare epidemics and better understand their epidemiological behavior.

`epifitter` wraps those workflows into a package-oriented interface that includes model fitting, summary measures, simulation helpers, and plotting functions tailored to plant disease epidemiology.

Current functionality includes:

-   Fit classic population dynamics models using linear and nonlinear approaches
-   Select models based on statistical and visual analysis
-   Calculate the area under the disease progress curve
-   Compare epidemics via visual inference
-   Simulate synthetic epidemics of various shapes and uncertainty

## Why use epifitter?

-   Compare the same epidemic against multiple canonical disease progress models.
-   Move from simulation to fitting with a consistent data structure.
-   Work with single epidemics or grouped data using the same package vocabulary.
-   Produce `ggplot2`-ready outputs for reports, papers, and teaching material.

## Quick start

Install the stable release from CRAN:

``` {.r}
install.packages("epifitter")
```

Install the development version from GitHub with `pak`:

``` {.r}
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

pak::pak("AlvesKS/epifitter")
```

## Example

``` {.r}
library(epifitter)

set.seed(1)
epi <- sim_logistic(N = 30, y0 = 0.01, dt = 5, r = 0.3, alpha = 0.2, n = 4)
fit <- fit_lin(time = epi$time, y = epi$y)

plot_fit(fit)
```

For more complete workflows, see the package articles on model fitting, area summaries, simulation, and the bundled experimental dataset in the documentation site.

## Article and citation

`epifitter` is described in the following paper:

Alves, K. S., & Del Ponte, E. M. (2021). Analysis and simulation of plant disease progress curves in R: introducing the epifitter package. *Phytopathology Research*, 3, 22. [https://doi.org/10.1186/s42483-021-00098-7](https://doi.org/10.1186/s42483-021-00098-7)

If you use `epifitter` in research, please cite the article above. You can also retrieve the package citation directly in R with `citation("epifitter")`. A BibTeX entry is shown below:

```bibtex
@article{Alves2021epifitter,
  author = {Alves, Kaique S. and Del Ponte, Emerson M.},
  title = {Analysis and simulation of plant disease progress curves in R: introducing the epifitter package},
  journal = {Phytopathology Research},
  year = {2021},
  volume = {3},
  number = {1},
  pages = {22},
  doi = {10.1186/s42483-021-00098-7},
  url = {https://doi.org/10.1186/s42483-021-00098-7}
}
```

## Meta

-   Please [report any issues or bugs](https://github.com/AlvesKS/epifitter/issues).
-   All code is licensed MIT
-   To cite epifitter, please use the output from citation("epifitter")
-   Please note that epifitter is released with [Contributor Code of Conduct](https://alvesks.github.io/epifitter/CODE_OF_CONDUCT.html). By participating in this project you agree to abide by its terms.
