\name{fit_nlin2}
\alias{fit_nlin2}
\title{Fits epidemic models using nonlinear aproach. This function also estimates the maximum disease intensity parameter K}
\usage{
fit_nlin2(time,
  y,
  starting_par = list(y0 = 0.01, r = 0.03, K =  0.8),
  maxiter = 50)
}
\description{
Fits epidemic models (Exponential, Monomolecular, Logistic and Gompertz) using nonlinear approach for estimate parameters. This function also estimates the maximum disease intensity parameter K.
}
\arguments{
\item{time}{Numeric vector which refers to the time steps in the epidemics.}
\item{y}{Numeric vector which refers to the disease intensity.}
\item{starting_par}{starting value for initial inoculun (y0) and apparent infection rate (r), and maximum disease intensity (K). Please informe in that especific order}
\item{maxiter}{Maximun number of iterations.}
}
\examples{

set.seed(1)

epi1 <- sim_logistic(N = 30,
                     y0 = 0.01,
                     dt = 5,
                     r = 0.3,
                     alpha = 0.5,
                     n = 4)

data = data.frame(time =  epi1[,2], y = epi1[,4])
fit_nlin2(time = data$time,
          y =  data$y,
          starting_par = list(y0 = 0.01, r = 0.03, K = 1),
          maxiter = 1024)
}
