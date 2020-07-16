\name{fit_nlin2}
\alias{fit_nlin2}
\title{Fits epidemic models using nonlinear aproach. This function also estimates the maximum disease intensity parameter K}
\usage{
fit_nlin2(time,
  y,
  guess_y0,
  guess_r,
  guess_K,
  maxiter = 50)
}
\description{
Fits epidemic models (Exponential, Monomolecular, Logistic and Gompertz) using nonlinaer aproach for estimate parameters. This function also estimates the maximum disease intensity parameter K.
}
\arguments{
\item{time}{Numeric vector which refers to the time steps in the epidemics.}
\item{y}{Numeric vector which refers to the disease intensity.}
\item{guess_y0}{starting value for initial inoculun.}
\item{guess_r}{starting value for apparent infection rate.}
\item{maxiter}{Maximun number of iterations.}
\item{guess_K}{starting value for Maximun disease intensity parameter.}
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
          guess_y0 = 0.001,
          guess_r = 0.3,
          guess_K = 1,
          maxiter = 1024)
}