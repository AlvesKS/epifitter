# *epifitter* <img src="man/figures/logo.png" width = 150px align="right"/>

[![CRAN](https://www.r-pkg.org/badges/version/epifitter)](https://CRAN.R-project.org/package=epifitter) [![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/epifitter)](https://CRAN.R-project.org/package=epifitter)

Provides a set of tools for aiding in the **visualization, description, and comparison of plant disease progress curve** (DPC) data. A DPC depict the change in a disease-intensity variable measured sequentially at different times during the epidemics. Their analysis, that may include fitting "classic" population dynamics models (e.g. logistic, monomolecular, Gompertz), allows gaining understanding of the epidemiological processes, but is most used for comparing epidemics.

The mathematics behind model fitting is straightforward and general-purpose spreadsheet or statistical softwares can be used to perform the calculations and select the "best" model. What `epifitter` does is to provide the analyst with R functions for performing several tasks commonly used for the temporal analysis of epidemics, including graphical output.

Current implementation includes functions for the analyst to:

-   Fit classic population dynamics models using linear and nonlinear approaches
-   Select models based on statistical and visual analysis
-   Calculate the area under the disease progress curve
-   Compare epidemics via visual inference
-   Simulate synthetic epidemics of various shapes and uncertainty

## Quick start

Install the stable release from CRAN.

``` {.r}
install.packages("epifitter")
```

The development version of **epifitter** is available from GitHub. The **devtools** package, available from CRAN, is required for installation.

``` {.r}
if (!require(devtools)) {
  install.packages("devtools")
}

devtools::install_github("AlvesKS/epifitter")
```

## Meta

-   Please [report any issues or bugs](https://github.com/AlvesKS/epifitter/issues).
-   All code is licensed MIT
-   To cite epifitter, please use the output from citation("epifitter")
-   Please note that epifitter is released with [Contributor Code of Conduct](https://alvesks.github.io/epifitter/CODE_OF_CONDUCT.html). By participating in this project you agree to abide by its terms.
