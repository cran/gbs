`searchMode` <-
function(x){

    x <- unlist(x)
    x <- x[!is.na(x)]
    u <- unique(x)
    frequencies <- rep(0, length(u))

    for(i in seq(along = u)){
        count <- 0

        for(j in seq(along = x)){
            if(u[i] == x[j]){
                count <- (count + 1)
            }
        }
        frequencies[i] <- count
    }

    maximumFreq  <- max(frequencies)
    indexMaximum <- which(frequencies == maximumFreq)
    modeResult   <- sort(x = u[indexMaximum], decreasing = FALSE)

    if(length(x) == length(u)){
        modeResult <- maximumFreq <- NA
    }

    results <- list(modalValue  = modeResult,
                    frequencies = maximumFreq)
    return(results)
}

