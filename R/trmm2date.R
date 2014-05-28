
#' Date parser for TRMM file names
#' 
#' @param x Character or list of characters. A typical TRMM file name(s)
#' 
#' @return A date object
#' @author Loic Dutrieux
#' @import stringr
#' @export

trmm2date <- function(x) {
    dates <- as.Date(str_extract(basename(x), '\\d{4}\\.\\d{2}\\.\\d{2}'), format='%Y.%m.%d')
    return(dates) 
}