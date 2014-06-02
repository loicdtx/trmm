
#' Aggregation function for TRMM data to monthly data
#' 
#' @description Creates monthly composites from daily trmm data
#' 
#' @param x filename, rasterStack, rasterBrick, or list of rasterLayers (filenames)
#' @param dates (optional) A date vector (See \code{\link{trmm2date}} to extract dates from trmm filenames).  If no date vector is supplied, the function will try to extract the time onformation from the z dimension of the raster object (if x is a RasterStack or RasterBrick)
#' @param FUN The compositing function (sum by default)
#' @param ... Arguments to be passed to \code{\link{writeRaster}}
#' 
#' @return RasterStack with time written to the z dimention
#' @author Loic Dutrieux
#' 
#' @import zoo
#' @import raster
#' 
#' @examples
#' data(dailyTRMM)
#' monthly <- timeAggregateMonth(daily)
#' 
#' @export

timeAggregateMonth <- function(x, dates=NULL, FUN=sum, ...) {
    # x is a filename, rasterStack, rasterBrick, or list of rasterLayers (filenames)
    if(is.character(x)){
        if (length(x) == 1) {
            x <- brick(x)
        } else {
            x <- stack(x)
        }
    }
    
    if(is.null(dates)) {
        dates <- getZ(x)
        if (is.null(dates)){
            stop("The date information is nowhere to be found!!!")
        }
            
    }
    
    datesA <- as.Date(as.yearmon(dates))
    
    # Dummy ts vector to return aggregated time vector
    dummy <- zoo(rep(NA, length(dates)), dates)
    dummyA <- aggregate(dummy, datesA, FUN=FUN)
    datesOut <- time(dummyA)
    
    fun2 <- function(x) {
        ts <- zoo(as.vector(x), dates)
        tsOut <- aggregate(ts, datesA, FUN=FUN)
        return(as.numeric(tsOut))
    }
    
    out <- calc(x=x, fun=fun2, ...)
    out <- setZ(out, datesOut)
    
    if (hasArg(filename)) {
        hdr(out, format='RASTER')
    }
    
    return(out)
    
}