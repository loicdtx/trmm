
#' Retrieve projection and Extent information from raster* object
#' 
#' @description Used mostly by \code{\link{warpModis2Landsat}} to retrieve info for reprojection.
#' @param x Raster* object from which parameters are to be retrieved
#' @param buffer Numeric or \code{NULL}. If numeric, the padding distance around the actual extent of the dataset to be added to the returned extent (in crs unit). 
#' @param multiple Logical. For the buffer to be rounded to a multiple of x's resolution.
#' @return A list containing projection, extent, and resolution as well as some already gdal formated arguments
#' @author Loic Dutrieux
#' @import plyr
#' @import raster
#' 
#' 

.getProj <- function(x, buffer=NULL, multiple=TRUE) {
    
    if (is.character(x)) {
        x <- brick(x)
    }
        
    
    if(is.null(buffer)) {
        buffer <- 0
    }
    
    if(multiple){
        buffer <- round_any(buffer, res(x)[1])
    }
    
    xmin <- x@extent@xmin - buffer
    ymin <- x@extent@ymin - buffer
    xmax <- x@extent@xmax + buffer
    ymax <- x@extent@ymax + buffer
    proj4 <- x@crs@projargs
    proj44gdal <- sprintf('-t_srs \"%s\"', proj4)
    Extent4gdal <- sprintf('-te %d %d %d %d', xmin, ymin, xmax, ymax)
    resolution <- res(x)[1]
    return(list(proj4 = proj4, proj44gdal = proj44gdal, Extent4gdal = Extent4gdal, resolution=resolution))
}