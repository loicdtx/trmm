
#' Aggregation function for TRMM data
#' 
#' @description Aggregate TRMM data over time, using a defined function. Also see \code{\link{trmmCompose}} which does something different.
#' 
#' @param x filename, rasterStack, rasterBrick, or list of rasterLayers (filenames)
#' @param dates A date vector (See \code{\link{trmm2date}} to extract dates from trmm filenames)
#' @param by Numeric. The lenght of the aggregation period
#' @param FUN The compositing function
#' @param ... Arguments to be passed to \code{\link{writeRaster}}
#' 
#' @return An object of class spaceTime
#' @author Loic Dutrieux
#' @import zoo
#' @import raster
#' 
#' @export

timeAggregate <- function(x, dates, by, FUN=sum, ...) {
    # x is a filename, rasterStack, rasterBrick, or list of rasterLayers (filenames)
    if(is.character(x)){
        if (length(x) == 1) {
            x <- brick(x)
        } else {
            x <- stack(x)
        }
    }
    
    # Dummy ts vector to return aggregated time vector
    dummy <- zoo(rep(NA, length(dates)))
    dummyA <- aggregate(dummy, dates - as.numeric(dates) %% by, FUN=FUN)
    dateOut <- time(dummyA)
    
    fun2 <- function(x) {
        ts <- zoo(as.vector(x), dates)
        tsOut <- aggregate(ts, dates - as.numeric(dates) %% by, FUN=FUN)
        return(as.numeric(tsOut))
    }
    
    out <- calc(x=x, fun=fun2, ...)
    
    if(hasArg(filename)) {
        dots <- list(...)
        time <- dateOut
        extension(dots$filename) <- '.rda'
        save(time, file=dots$filename)
    }
    
    
    
    out <- list(data = out, 
                time = dateOut)
    
    class(out) <- 'spaceTime'
    
    return(out)
    
}