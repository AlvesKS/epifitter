\name{AUDPS}
\alias{AUDPS}
\title{
Area under disease progress stairs
}
\usage{
AUDPS(time, y, y_proportion, type = "absolute")
}
\description{
Calculates the area under disease progress stairs.
}
\arguments{
  \item{time}{
A vector object of time.
}
  \item{y}{
A vector object of disease intensity.
}
  \item{y_proportion}{
Logical. If disease intensity is proportion (TRUE) or percentage(FALSE)
}
  \item{type}{
Set if is absolute or relative AUDPC. type = "absolute" is default.
}
}

\references{
Simko, I., and Piepho, H.-P. 2012. The area under the disease progress stairs: Calculation, advantage, and application. Phytopathology 102:381- 389. 
}
\author{
Kaique dos S. Alves
}

\examples{
epi =  sim_logistic(N = 30, y0 = 0.01,dt = 5, r = 0.3, alpha = 0.5, n = 1)
AUDPS(time = epi$time, y = epi$y, y_proportion = TRUE)

}

