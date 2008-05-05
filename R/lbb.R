`lbb` <-
function(theta, x, nu = 1.0, kernel = "normal"){
            ker   <- kernel
            alpha <- theta[1]
            beta  <- theta[2]
            nu    <- nu
            argu  <- kappaii(x, c(alpha, beta))
            vi    <- (-2) * (wg(argu, nu, ker))
            vip   <- (-2)*(wgp(argu, nu, ker))
            n     <- length(x)
            a     <- ((1 / x) - (x / beta^2))
            b     <- (x + beta)^(-2)
            c     <- sum(b)
            d     <- vip*a^2
            e     <- sum(d)
            f     <- sum(vi*x)
            g     <- (n / (2 * beta^2)) - c - ((1 / (2 * alpha^4)) * e) - 
                     (1 / ((alpha^2) * (beta^3))) * f
            return(g)
    }

