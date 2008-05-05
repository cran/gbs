`la` <-
function(theta, x, nu = 1.0, kernel = "normal"){
       ker   <- kernel
       alpha <- theta[1]
       beta  <- theta[2]
       nu    <- nu
       argu  <- kappaii(x, c(alpha, beta))
       vi    <- (-2) * (wg(argu, nu, ker))
       a     <- ((x / beta) + (beta / x) - 2) / (alpha^3)
       b     <- vi * a
       c     <- sum(b) - (n / alpha)
       return(c)
   }

