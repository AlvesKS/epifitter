#' Powdery mildew disease progress curves in organic tomato
#'
#' Experimental disease progress curve data for powdery mildew under different
#' irrigation systems and soil moisture levels in organic tomato.
#'
#' @docType data
#' @name PowderyMildew
#' @keywords datasets
#'
#' @format A data frame with 240 rows and 5 variables:
#' \describe{
#'   \item{irrigation_type}{Irrigation system.}
#'   \item{moisture}{Soil moisture level.}
#'   \item{block}{Experimental block.}
#'   \item{time}{Assessment time.}
#'   \item{sev}{Disease severity as a proportion.}
#' }
#'
#' @references
#' Lage, D. A. C., Marouelli, W. A., and Cafe-Filho, A. C. (2019).
#' Management of powdery mildew and behaviour of late blight under different
#' irrigation configurations in organic tomato. \emph{Crop Protection}, 125,
#' 104886.
#'
#' @examples
#' data("PowderyMildew")
#' str(PowderyMildew)
NULL
