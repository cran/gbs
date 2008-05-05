`mlegbstc` <-
function(x, nuFixed = 2.0, status){

         if(!is.numeric(x))
           {stop("non-numeric argument to mathematical function")}
	  
         initials   <- mlebs(x) 
         thetaStart <- c(initials$alphabsEstimate, initials$betabsEstimate)
         delta      <- status

         loglik     <- function(theta, x){

            sum( - (delta) * dgbs(x, alpha = theta[1], beta = theta[2],
                nu = nuFixed, kernel = "t", log = TRUE)-
                (1 - delta) * log(rfgbs(x, alpha = theta[1], beta = theta[2],
                 nu = nuFixed, kernel = "t")))
          }

         maximization <- nlm(loglik, thetaStart, x = x)
         estimates    <- maximization$estimate
         results      <- list(alphaEstimate = estimates[1],
                              betaEstimate  = estimates[2],
			            logLikelihood = loglik(estimates, x))
   return(results)
  }

