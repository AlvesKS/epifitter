\name{fit_lin}
\alias{fit_lin}
\title{Fits epidemic models using data linearization}
\usage{
fit_lin(time,y)
}
\description{
Fits epidemic models (Exponential, Monomolecular, Logistic and Gompertz) to data using data linearization
}
\arguments{
\item{time}{Numeric vector which refers to the time steps in the epidemics}
\item{y}{Numeric vector which refers to the disease intensity}
}
\author{
Kaique dos S. Alves
}
\examples{
set.seed(1)
epi1 <- sim_logistic(N = 30,
                     y0 = 0.01,
                     dt = 5,
                     r = 0.3,
                     alpha = 0.2,
                     n = 4)
data = data.frame(time =  epi1[,2], y = epi1[,4])
fit_lin( time = data$time, y =  data$y)

}
