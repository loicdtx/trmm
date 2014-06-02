
#' Reprojects a dataset to the projection of another one
#' 
#' @description Reprojects a dataset to the projection of another one allowing the user to set a buffer and pixel alignment (even if resolution is different)
#' 
#' @param target Raster* object.
#' @param trmm Raster object written to file and that should be re-projected to the target crs.
#' @param filename Character. Output filename (must be a gdal supported format)
#' @param buffer Numeric. Padding around the target extent, in crs units
#' @param multiple Logical. Should the output resolution be a multiple of the target's resolution?
#' @param run Logical. SHould the command be ran (gdalwarp via system call). Otherwise it just creates the gdalwarp string.
#' 
#'
#' @return Object of class spaceTime or character
#' @import raster
#' @import plyr
#' @export
#' 
#' 

warpTrmm2Raster <- function(target, trmm, filename, buffer=100000, multiple=TRUE, run=TRUE) {

    
    res <- res(brick(trmm))[1] * 100000 # 100000 is for a rough conversion from degree to meters
    ProjInfo <- .getProj(x = target, buffer=buffer) #multiple=TRUE is implicit
    if (multiple) {
        res <- round_any(res, ProjInfo$resolution)
    }
    te <- ProjInfo$Extent4gdal
    srs <- ProjInfo$proj44gdal
    WarpString <- sprintf('gdalwarp %s %s -tr %d %d %s %s', te, srs, res, res, trmm, filename)
    if(run){
        system(WarpString)
        return(filename)
    }
    
    return(WarpString)
}