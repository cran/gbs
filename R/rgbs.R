`rgbs` <-
function(n,
                 alpha  = 1.0,
                 beta   = 1.0,
                 nu     = 1.0,
                 kernel = "normal"){
     if(n == 0)     stop("Value of n must be greater or equal then 0")
     if(alpha <= 0) stop("alpha must be positive")
     if(beta <= 0)  stop("beta must be positive")
     if(kernel=="normal")

     {
      z <- rnorm(n, 0, 1)
     }

     if(kernel == "t")
     {
      nu <- nu
      z  <- rt(n, nu, ncp = 0)
     }
     if(kernel == "logistic")
     {
      z <- rlogis(n, 0, 1)
     }
     if(kernel == "laplace")
     {
      nu <- 1/2
      z  <- rgamma(n, 1/2, 1/nu)
     }
     t <- beta * (1 + (((alpha^2) * (z^2)) / 2) + (alpha * z * sqrt((((alpha^2) * (z^2)) / 4) + 1)))
     return(t)
   }

