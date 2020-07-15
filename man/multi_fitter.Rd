\name{multi_fitter}
\alias{multi_fitter}
\title{Estimate model parameters for multiple disease progress curves
}
\description{
Estimate model parameters for multiple disease progress curves
}
\usage{
multi_fitter(time_col,
             intensity_col,
             data,
             strata_cols,
             guess_y0,
             guess_r,
             guess_K,
             maxiter,
             nlin = FALSE,
             estimate_K = FALSE)
}

\arguments{
  \item{time_col}{
Character name specifying the culumn for the time. eg: time_col = "days".
}
  \item{intensity_col}{
Character name specifying the culumn for the disease intensity.
}
  \item{data}{
\code{data.frame} object containing the variables for model fitting.
}
  \item{strata_cols}{
Character name or vector specifying the culumns for stratification.
}
\item{guess_y0}{
Starting value for initial inoculun parameter.
}
\item{guess_r}{
 Starting value for apparent infection rate parameter.
}
\item{guess_K}{
 starting value for Maximun disease intensity parameter. If you want to estmate K, please set \code{estimate_K = TRUE}
}
\item{maxiter}{
 Maximun number of iterations. Only used if is \code{nlin = TRUE}
}
\item{nlin}{
 Logical. If \code{FALSE} estimates parameters using data linearization. If \code{nlin=TRUE}, estimates nonlinear aproach. \code{\link{fit_nlin}}.
}
\item{estimate_K}{
 Logical. If \code{nlin=TRUE}, estimates maximum disease intensity. (default: \code{nlin=FALSE}) \code{\link{fit_nlin2}}.
}
}
\value{
Returns a \code{data.frame} containg estimated parameters for individual strata levels.
}

\seealso{
\code{\link{fit_lin}},\code{\link{fit_nlin}},\code{\link{fit_nlin2}}
}
\examples{
set.seed(1)
# create stratified dataset
data_A1 = sim_gompertz(N = 30, y0 = 0.01,dt = 5, r = 0.3, alpha = 0.5, n = 4)
data_A1 = dplyr::mutate(data_A1,
                        fun = "A",
                        cultivar = "BR1")
set.seed(1)
data_B1 = sim_gompertz(N = 30, y0 = 0.01, dt = 5, r = 0.2, alpha = 0.5, n = 4)
data_B1 = dplyr::mutate(data_B1,
                        fun = "B",
                        cultivar = "BR1")
set.seed(1)
data_A2 = sim_gompertz(N = 30, y0 = 0.01,dt = 5, r = 0.1, alpha = 0.5, n = 4)
data_A2 = dplyr::mutate(data_A2,
                        fun = "A",
                        cultivar = "BR2")
set.seed(1)
data_B2 = sim_gompertz(N = 30, y0 = 0.01,dt = 5, r = 0.1, alpha = 0.5, n = 4)
data_B2 = dplyr::mutate(data_B2,
                        fun = "B",
                        cultivar = "BR2")

data = dplyr::bind_rows(data_A1, data_B1,data_A2, data_B2)

multi_fitter(time_col = "time",
             intensity_col = "random_y",
             data = data,
             strata_col = c("fun","cultivar"),
             guess_y0 = 0.01,
             guess_r = 0.3,
             maxiter = 1024,
             nlin = FALSE,
             estimate_K = FALSE)
}

