AUDPC_2_points <- function(time, y0, yT) {
    # eqn. 3 from Jeger and Viljanen-Rollinson (2001), rate parameter r
    r <- (log(yT / (1 - yT)) - log(y0 / (1 - y0))) / time

    # eqn. 2 from Jeger and Viljanen-Rollinson (2001), AUDPC estimation
    return(time + log(y0 / yT) / r)
}
