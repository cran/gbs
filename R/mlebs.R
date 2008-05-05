`mlebs` <-
function(x){
    if(!is.numeric(x))
    {stop("non-numeric argument to mathematical function")}
    t <- x
    n <- length(t)
    s <- (1/n) * sum(t)
    r <- ((1/n) * sum(t^(-1)))^(-1)
    K <- function(x)
    {
     a1 <- ((x + t)^(-1))
     a2 <- ((1/n)*sum(a1))^(-1)
     return(a2)
    }
    Kd <- function(x){
    a3 <- ((x + t)^(-2))
    a4 <- ((1/n) * sum(a3))
    a5 <- ((K(x))^2) * a4
    return(a5)
   }
    g   <- function(x){
    res <- (x^2) - (x*((2 * r) + K(x))) + (r * (s + K(x)))
    return(res)
   }
    gd  <- function(x){
    b1  <- (x - r) * (1 - Kd(x))
    b2  <- (b1 + x - r - K(x))
    return(b2)
   }
    mm  <- sqrt(s * r)
    tol <- 0.00000001
    val <- 5
    xn  <- mm
    res  <- 1
    cont <- 0
    while(val > tol){
                    res  <- (xn - (g(xn) / gd(xn)))
                    val  <- (abs(res - xn) / xn)
                    xn   <- res
                    cont <- cont + 1
    }
    minimum   <- min(t)
    converge  <- c("FALSE")
    if((2*s)  <= (3 * r + minimum)){converge <- c("TRUE")}
    bet       <- res
    alph      <- sqrt(((s / bet) + (bet / r) - 2))
    estimates <- list(betaStart = mm, alphabsEstimate = alph, betabsEstimate = bet, converge = converge,iteration = cont)

    return(estimates)

   }

