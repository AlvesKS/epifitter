#' Estimate disease progress curve from only two points
#'
#' Estimates area under the disease progress curve \acronym{AUDPC} from two
#' qualifications only, see Jeger and Viljanen-Rollinson (2001) for more.
#'
#' @param time Number of days from first disease assessment to last disease
#' assessment.
#' @param y0 A numeric value representing the disease intensity at the first
#' intensity measurement.
#' @param yT A numeric value representing the disease intensity at the last
#' intensity measurement.
#'
#' @references Jeger, M. J., & Viljanen-Rollinson, S. L. H. (2001). The use
#' of the area under the disease-progress curve (AUDPC) to assess quantitative
#' disease resistance in crop cultivars. *Theoretical and Applied Genetics*,
#' 102, 32-40.
#'
#' @examples
#'
#' epi <- sim_logistic(N = 30, y0 = 0.01, dt = 5, r = 0.3, alpha = 0.5, n = 1)
#'
#' # compare with use values from `epi`, first and last only
#' # time
#' epi$time[7]
#'
#' # y0
#' epi$y[1]
#'
#' # yT
#' epi$y[7]
#'
#' audpc_2_points(time = epi$time[7], y0 = epi$y[1], yT = epi$y[7])
#'
#' # compare with traitional AUDPC trapezoidal method
#' AUDPC(time = epi$time, y = epi$y, y_proportion = TRUE)
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @export

audpc_2_points <- function(time, y0, yT) {
  # eqn. 3 from Jeger and Viljanen-Rollinson (2001), rate parameter r
  r <- (log(yT / (1 - yT)) - log(y0 / (1 - y0))) / time

  # eqn. 2 from Jeger and Viljanen-Rollinson (2001), AUDPC estimation
  return(time + log(y0 / yT) / r)
}
