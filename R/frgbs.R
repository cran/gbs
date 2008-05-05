`frgbs` <-
function(x,
                  alpha  = 1.0,
                  beta   = 1.0,
                  nu     = 1.0,
                  kernel = "normal"){

    hazard <- switch(kernel,
                "normal"   = {
                             dgbs(x, alpha, beta, nu = 1.0, "normal") /
                             (1 - pgbs(x, alpha, beta, nu = 1.0, "normal"))
                             },
                "t"        = {
                             dgbs(x, alpha, beta, nu, "t") /
                             (1 - pgbs(x, alpha, beta, nu, "t"))
                             },
                "laplace"  = {
                             dgbs(x, alpha, beta, nu = 1.0, "laplace") /
                             (1 - pgbs(x, alpha, beta, nu = 1.0, "laplace"))
                             },
                "logistic" = {
                             dgbs(x, alpha, beta, nu = 1.0, "logistic") /
                             (1 - pgbs(x, alpha, beta, nu = 1.0, "logistic"))
                             }
              )
    return(hazard)
}

