\name{PowderyMildew}
\alias{PowderyMildew}
\docType{data}
\title{
Dataset powdery mildew disease progress curves
}
\description{
Dataset containing experimental data of disease progress curves of powdery mildew under different irrigation systems and soil moisture levels in organic tomato.
}
\usage{data("PowderyMildew")}
\format{
  A data frame with 240 observations on the following 2 variables.
  \describe{
    \item{\code{irrigation_type}}{Irrigations Systems: MS = Micro Sprinkler}
    \item{\code{moisture}}{Levels of soils moisture}
    \item{\code{block}}{Experimental blocks}
    \item{\code{time}}{a numeric vector containing the time points}
    \item{\code{sev}}{a numeric vector containg disease severity data in proportinal scales}
  }
}
\references{
Lage, D. A. C., Marouelli, W. A., and Café-Filho, A. C. 2019. Management of powdery mildew and behaviour of late blight under different irrigation configurations in organic tomato. Crop Protection. 125:104886.

}
\examples{
data(PowderyMildew)
## maybe str(PowderyMildew) ; plot(PowderyMildew) ...
}
\keyword{datasets}
