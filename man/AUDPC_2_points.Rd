\name{AUDPC2_points}
\alias{AUDPC_2_points}
\title{Estimate disease progress curve from only two points}
\usage{
AUDPC_2_points(time, y0, yT)
}
\arguments{
\item{time}{Number of days from first disease assessment to last disease
assessment.}

\item{y0}{A numeric value representing the disease intensity at the first
intensity measurement. Must be greater than zero and less than one.}

\item{yT}{A numeric value representing the disease intensity at the last
intensity measurement. Must be greater than \code{y0} and less than one.}
}
\description{
Estimates area under the disease progress curve \acronym{AUDPC} from two
qualifications only, see Jeger and Viljanen-Rollinson (2001) for more.
}
\examples{

epi <- sim_logistic(N = 30, y0 = 0.01, dt = 5, r = 0.3, alpha = 0.5, n = 1)

# compare with use values from `epi`, first and last only
# time
epi$time[7]

# y0
epi$y[1]

# yT
epi$y[7]

AUDPC_2_points(time = epi$time[7], y0 = epi$y[1], yT = epi$y[7])

# compare with traitional AUDPC trapezoidal method
AUDPC(time = epi$time, y = epi$y, y_proportion = TRUE)
}
\references{
Jeger, M. J., & Viljanen-Rollinson, S. L. H. (2001). The use
of the area under the disease-progress curve (AUDPC) to assess quantitative
disease resistance in crop cultivars. *Theoretical and Applied Genetics*,
102, 32-40. DOI: \href{https://doi.org/10.1007/s001220051615}{10.1007/s001220051615}.
}
\author{
Adam H. Sparks, \email{adamhsparks@gmail.com}
}
