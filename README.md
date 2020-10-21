# *epifitter* <img width = 200px style = "margin-left: 40px; margin-top: 50px" align = right src="man/figures/logo.png" />

## Introduction

The __epifitter__ package provides a set of tools for aiding in the **visualization, description, and comparison of plant disease progress curve** (DPC) data. DPCs depict the change in a disease-intensity variable measured at different times during the epidemics. 


## Overview

Their analysis, that may include fitting "classic" population dynamics models (e.g. logistic, monomolecular, gompertz), allows gaining understanding of the epidemiological processes, but is also used widely for comparing epidemics.

The mathematics behind model fitting is straightforward and general-purpose spreadsheet or statistical softwares can be used to perform the calculations and select the "best" model. What `epifitter` does is to provide the analyst with R functions for performing several tasks commonly used for the temporal analysis of epidemics, including graphical output.

Current implementation includes functions for:

- Fitting classic population dynamics models using linear and nonlinear approaches
- Selecting best fitted models based on statistical and visual analysis
- Calculating the area under the disease progress cures (AUDPC)
- Comparing epidemics statistically
- Simulating synthetic epidemics of various shapes and uncertainty
- Producing publication-ready multiple panel plots


## How to install

Currently, only the development version of **epifitter** is available from GitHub. The  **devtools** package, available from CRAN, is required for installation.   


``` r
if (!require(devtools)) {
  install.packages("devtools")
}

devtools::install_github("AlvesKS/epifitter")
```


## Meta


Kaique S Alves, D.Sc Student  
Universidade Federal de Viçosa    
Viçosa MG Brazil
<https://alvesks.netlify.com/>

Emerson M Del Ponte, Assoc Professor  
Universidade Federal de Viçosa  
Viçosa MG Brazil  
<https://emersondelponte.netlify.com/>


## Contributing

This project is released with a [Contributor Code of Conduct](https://alvesks.github.io/epifitter/CODE_OF_CONDUCT.html). By participating in this project you agree to abide by its terms. If you wish to contribute code to **epifitter**, please fork the repository and create a pull request with your added feature.
