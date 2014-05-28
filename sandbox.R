library(SPEI)
library(bfast)

# Load data
data(wichita)
head(wichita)





fun <- function() {
    
    spi <- spi()
    out <- as.numeric(spi$fitted)
    return(out)
}


### Ongoing tests; results from a zoo objects and from a ts object look different... Why?