
#' Date parser for TRMM file names
#' 
#' @param x Character or list of characters. A typical TRMM file name(s)
#' 
#' @return A date object
#' @author Loic Dutrieux
#' 
#' @details Works for products downloaded using the \href{http://mirador.gsfc.nasa.gov/}{mirador} interface or directly from ftp (The three products supported by this function are 3B42, 3B42 daily, and 3B43)
#' 
#' @import stringr
#' @export

trmm2date <- function(x) {
    p3B42_daily <- '^.*(3B42_daily\\.)(\\d{4}\\.\\d{2}\\.\\d{2})\\.7(\\.SUB)?\\.(?i)(nc|bin|hdf)'
    p3B42 <- '^.*(3B42\\.)(\\d{8}\\.\\d{2})\\.7A?(\\.SUB)?\\.(?i)(nc|hdf)'
    p3B43 <- '^.*(3B43\\.)(\\d{8})\\.7A?(\\.SUB)?\\.(?i)(nc|hdf)'
    x <- basename(x)
    fun <- function(x){
        if(grepl(pattern=p3B42_daily, x)) {
            dates <- as.Date(str_match(x, pattern=p3B42_daily)[,3], format='%Y.%m.%d')
        } else if(grepl(pattern=p3B42, x)) { # Not needed at the moment and more complex to deal with the output together with other returns
            # dates <- strptime(str_match(x, pattern=p3B42)[,3], format='%Y%m%d.%H')
            dates <- NA
        } else if(grepl(pattern=p3B43, x)) {
            dates <- as.Date(str_match(x, pattern=p3B43)[,3], format='%Y%m%d')
        } else {
            dates <- NA
        }
        return(dates)
    }
    do.call(c, lapply(x, fun))
}