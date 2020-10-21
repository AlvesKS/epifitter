\name{plot_fit}
\alias{plot_fit}
\title{Creates a plot panel for the fitted models}
\usage{
plot_fit(object,
  point_size =1.2,
  line_size = 1,
  models = c("Exponential","Monomolecular", "Logistic", "Gompertz"))
}
\description{
Create a \code{ggplot2}-style plot with the fitted models curves and the epidemic data.
}
\arguments{
\item{object}{A \code{fit_lin} or a \code{fit_nlin} object}
\item{point_size}{Point size}
\item{line_size}{Line size}
\item{models}{Select the models to be displayed in the panel}
}
\details{
It is possible to add more \code{ggplot} components by using the \code{+} syntax. See examples below.

}

\examples{
epi1 <- sim_logistic(N = 30,
                     y0 = 0.01,
                     dt = 5,
                     r = 0.3,
                     alpha = 0.5,
                     n = 4)
data = data.frame(time =  epi1[,2], y = epi1[,4])
fitted = fit_lin( time = data$time, y =  data$y)
plot_fit(fitted)

# adding ggplot components
library(ggplot2)
plot_fit(fitted)+
  theme_minimal()+
  ylim(0,1)+
  labs(y = "Disease internsity", x = "Time")
}
