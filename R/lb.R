`lb` <-
function(theta, x, nu = 1.0, kernel = "normal"){
       ker   <- kernel
       alpha <- theta[1]
       beta  <- theta[2]
       nu    <- nu
       argu  <- kappaii(x, c(alpha, beta))
       vi    <- (-2) * (wg(argu, nu, ker))
       n     <- length(x)
       a     <- ((1 / x) - (x / beta^2)) / (2 * alpha^2)
       b     <- vi * a
       c     <- sum(1 / (x + beta)) - sum(b) - (n / (2 * beta))
      return(c)
   }

