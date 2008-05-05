`dgbspvii` <-
function(x,
                    alpha      = 1.0,
                    beta       = 1.0,
                    parameters = c(1.0, 1.0),
                    log        = FALSE){

    dpvii <- function(u, theta = c(1.0, 1.0)){

        qParameter <- theta[1]
        rParameter <- theta[2]
        nc         <- gamma(qParameter) /
                      (sqrt(rParameter * pi) * gamma(qParameter - (1 / 2)))
        gKernel    <- (1 + ((u ^ 2) / rParameter)) ^ (- qParameter)
        dsty       <- nc * gKernel
        return(dsty)
    }

    quantity <- (1 / alpha) * (sqrt(x / beta) - sqrt(beta/ x))
    jacobian <- x^(-3/2)*(x + beta) / (2*alpha*sqrt(beta))
    density  <- dpvii(quantity, parameters) * jacobian

    if(log == TRUE){
        density <- log(density)
    }
    return(density)
}

