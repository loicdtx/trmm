library(SPEI)
library(bfast)

# Load data
data(wichita)
head(wichita)

# Build a TRMM dataset
dir <- 'F:/RS/test/trmm/'
list <- list.files(dir, pattern='^.*\\.nc', full.names=TRUE)
stack <- stack(list)

subStack <- crop(stack, e)

subStack2 <- setZ(x=subStack, z=trmm2date(list))
daily <- subStack2



fun <- function() {
    
    spi <- spi()
    out <- as.numeric(spi$fitted)
    return(out)
}


### Ongoing tests; results from a zoo objects and from a ts object look different... Why?