`laa` <-
function(theta, x, nu = 1.0, kernel = "normal"){
            ker   <- kernel
            alpha <- theta[1]
            beta  <- theta[2]
            nu    <- nu
            argu  <- kappaii(x, c(alpha, beta))
            vi    <- (-2) * (wg(argu, nu, ker))
            vip   <- (-2)*(wgp(argu, nu, ker))
            n     <- length(x)
            a     <- ((x / beta) + (beta / x) - 2)
            b     <- vi * a
            c     <- sum(b)
            d     <- vip*a^2
            e     <- sum(d)
            f     <- (n / alpha^2) - ((2 / alpha^6) * e) - (3 / alpha^4) * c
            return(f)
   }

