
#' Compositing function for TRMM data
#' 
#' @description Creates composites that can be matched with each others from one year to the other. The same way that MODIS composites work. It differs from \code{\link{timeAggregate}} by the fact that aggregated periods can be matched between years. However, warning because the last composite of the year is likely to be truncated (particularly relevant in case of cumulative value composite, which is the default here)
#' 
#' @param x filename, rasterStack, rasterBrick, or list of rasterLayers (filenames)
#' @param dates A date vector (See \code{\link{trmm2date}} to extract dates from trmm filenames)
#' @param by Numeric. The lenght of the composite in days
#' @param FUN The compositing function
#' @param ... Arguments to be passed to \code{\link{writeRaster}}
#' 
#' @return A RasterStack with time written to the z dimension
#' @author Loic Dutrieux
#' @import zoo
#' @import raster
#' @import lubridate
#' 
#' @export



trmmCompose <- function(x, dates, by, FUN=sum, ...) {
    # x is a filename, rasterStack, rasterBrick, or list of rasterLayers (filenames)
    if(is.character(x)){
        if (length(x) == 1) {
            x <- brick(x)
        } else {
            x <- stack(x)
        }
    }
    
    
    # Function definition
    composeDate <- function(time, by) {
        y <- as.Date(cut(range(time), "years"))
        yd <- seq(y[1], y[2], "year")
        yd <- year(yd)
        fun <- function(x) {
            subTime <- time[format(time, "%Y") == as.character(x)]
            return(subTime - (which(subTime == subTime) %% by - 1))
        }
        out <- sapply(X=yd, FUN=fun, simplify=TRUE)
        do.call(c, out)
    }
    
    agrInd <- composeDate(time=dates, by=by) 
    
    
    
    # Dummy ts vector to return aggregated time vector
    dummy <- zoo(rep(NA, length(dates)))
    dummyA <- aggregate(dummy, by=agrInd, FUN=FUN)
    dateOut <- time(dummyA)
    
    fun2 <- function(x) {
        ts <- zoo(as.vector(x), dates)
        tsOut <- aggregate(ts, by=agrInd, FUN=FUN)
        return(as.numeric(tsOut))
    }
    
    out <- calc(x=x, fun=fun2, ...)
    out <- setZ(out, dateOut)
    
    if (hasArg(filename)) {
        hdr(out, format='RASTER')
    }
    
    return(out)
    
}