
#' Date parser for TRMM file names
#' 
#' @param x Character. A typical TRMM file name
#' 
#' @return A date object
#' @author Loic Dutrieux
#' @import stringr
#' @export

trmm2date <- function(x) {
    dates <- as.Date(str_extract(basename(x), '\\d{4}\\.\\d{2}\\.\\d{2}'), format='%Y.%m.%d')
    return(dates) 
}