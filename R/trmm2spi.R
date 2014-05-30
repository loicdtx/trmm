#' Compute spi
#' 
#' @description Compute spi from a rasterStack/Brick of monthly rainfall data 
#' 
#' @param x filename, rasterStack, rasterBrick, with time written on the z dimension
#' @param scale See \code{\link{spi}}
#' @param kernel See \code{\link{spi}}
#' @param distribution See \code{\link{spi}}
#' @Param fit See \code{\link{spi}}
#' @param na.rm See \code{\link{spi}}
#' @param ref.start See \code{\link{spi}}
#' @param ref.end See \code{\link{spi}}
#' @param keep See \code{\link{spi}} (called x in the original \code{\link{spi}} function)
#' @param param See \code{\link{spi}}
#' @param ... Arguments to be passed to \code{\link{writeRaster}}
#' 
#' @return RasterStack with time written to the z dimention
#' @author Loic Dutrieux
#' 
#' @import zoo
#' @import raster
#' @import SPEI
#' 
#' @examples
#' monthly <- timeAggregateMonth(daily)
#' spiSpatial <- trmm2spi(monthly, scale=6)
#' 
#' # Plot pixel number 2 of that brick
#' plot(zoo(as.vector(spiSpatial[2]), getZ(spiSpatial)))
#' 
#' @export




trmm2spi <- function(x, scale, kernel = list(type = 'rectangular', shift = 0),
                     distribution = 'Gamma', fit = 'ub-pwm', na.rm = FALSE,
                     ref.start=NULL, ref.end=NULL, keep=FALSE, params=NULL, ...) {
    
    if(is.character(x)) {
        x <- brick(x)
    }
    
    
    time <- getZ(x)
    if(is.null(time)) {
        stop("Please provide a raster object with time written to the z dimension")
    }
    
    fun <- function(x) {
        data <- zoo(x,time)
        spi <- spi(data=data, scale=scale, kernel=kernel, distribution=distribution, fit=fit, na.rm=na.rm, ref.start=ref.start, ref.end=ref.end, x=keep, params=params)
        spiOut <- as.vector(spi$fitted)
        return(spiOut)
    }
    
    out <- calc(x=x, fun=fun, ...)
    out <- setZ(x=out, z=time)
    
    if (hasArg(filename)) {
        hdr(out, format='RASTER')
    }
    
    return(out)
    

    
}