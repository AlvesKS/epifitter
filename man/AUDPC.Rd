\name{AUDPC}
\alias{AUDPC}
\title{
Area under disease progress curve
}
\usage{
AUDPC(time, y, y_proportion = TRUE, type = "absolute")
}
\description{
Calculates the area under disease progress curves.
}

\arguments{
  \item{time}{
A vector object of time.
}
  \item{y}{
A vector object of disease intensity.
}
  \item{y_proportion}{
Logical. If disease intensity is proportion (TRUE) or percentage(FALSE).
}
  \item{type}{
Set if is absolute or relative AUDPC. type = "absolute" is default.
}
}

\references{
Madden, L. V., Hughes, G., and van den Bosch, F. 2007. The Study of Plant Disease Epidemics. American Phytopathological Society, St. Paul, MN.
}
\author{
Kaique dos S. Alves
}

\examples{
epi =  sim_logistic(N = 30, y0 = 0.01,dt = 5, r = 0.3, alpha = 0.5, n = 1)
AUDPC(time = epi$time, y = epi$y, y_proportion = TRUE)

}

