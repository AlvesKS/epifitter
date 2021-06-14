\name{sim_gompertz}
\alias{sim_gompertz}
\title{Simulate an epidemic using the Gompertz model}
\usage{
sim_gompertz(N = 10,dt = 1, y0 = 0.01, r, K = 1, n,  alpha = 0.2)
}
\description{
Simulate a stochastic epidemic curve using the Gompertz model.
}

\arguments{
\item{N}{Total time course of the epidemic}
\item{dt}{Time step}
\item{y0}{Initial inoculum or initial disease intensity}
\item{r}{Infection rate}
\item{K}{Maximum asymptote}
\item{n}{Number or replicates or sample size for each time step}
\item{alpha}{Variation parameter. stands for the variation for the replicates for each time step. The standard deviation is calculated as sd = alpha * y * (1 - y), being y the disease intensity for each time step.}
}
\value{
\item{rep}{Replicates}
\item{time}{Time after epidemic start}
\item{y}{Disease intensity}
\item{random_y}{Disease intensity after applying the random \code{alpha} error}
}
\examples{
sim_gompertz(N = 30, y0 = 0.01,dt = 5, r = 0.3, K = 1, alpha = 0.5, n = 4)
}
