`rfgbs` <-
function(x,
                  alpha  = 1.0,
                  beta   = 1.0,
                  nu     = 1.0,
                  kernel = "normal"){

    reliability <- switch(kernel,
                          "normal"   = 1 - pgbs(x, alpha, beta, nu = 1.0,
                                                "normal"),
                          "t"        = 1 - pgbs(x, alpha, beta, nu, "t"),
                          "laplace"  = 1 - pgbs(x, alpha, beta, nu = 1.0,
                                                "laplace"),
                          "logistic" = 1 - pgbs(x, alpha, beta, nu = 1.0,
                                                "logistic")
                   )
    return(reliability)
    }

