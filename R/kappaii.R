`kappaii` <-
function(x, theta = c(1.0, 1.0)){

    alpha  <- theta[1]
    beta   <- theta[2]
    value  <- ((alpha^(-2)) * ((x / beta) + (beta / x) - 2))
    return(value)
}

