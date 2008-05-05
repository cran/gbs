`lab` <-
function(theta, x, nu = 1.0, kernel = "normal"){
            ker   <- kernel
            alpha <- theta[1]
            beta  <- theta[2]
            nu    <- nu
            argu  <- kappaii(x, c(alpha, beta))
            vi    <- (-2) * (wg(argu, nu, ker))
            vip   <- (-2)*(wgp(argu, nu, ker))
            vip   <- (-2) * (wgp(argu, nu, ker))
            n     <- length(x)
            a     <- ((x / beta) + (beta / x) - 2)
            b     <- ((1 / x) - (x / beta^2))
            c     <- vip * a * b
            d     <- sum(c)
            e     <- vi * b
            f     <- sum(e)
            g     <- (1 / alpha^3) * (((1 / alpha^2) * d) + f)
            return(g)
   }

