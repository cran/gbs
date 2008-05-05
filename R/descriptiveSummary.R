`descriptiveSummary` <-
function(x){

    n            <- length(x)
    skewness     <- (1 / n) * sum(((x - mean(x)) / sd(x)) ^ 3)
    kurtosis     <- ((1 / n) * sum(((x - mean(x)) / sd(x)) ^ 4)) - 3
    cVariation   <- (sd(x) / mean(x))
    modeCalculus <- searchMode(x)$modalValue
    statistics   <- list(mean                 = round(mean(x), 3),
                         median               = round(median(x), 3),
                         mode                 = round(modeCalculus, 3),
                         standardDeviation    = round(sd(x), 3),
                         coefficientVariation = round(cVariation * 100, 3),
                         coefficientSkewness  = round(skewness, 3),
                         coefficientKurtosis  = round(kurtosis, 3),
                         range                = round(max(x) - min(x), 3),
                         minimum              = round(min(x), 3),
                         maximum              = round(max(x), 3),
                         n                    = length(x))
    return(statistics)
}

