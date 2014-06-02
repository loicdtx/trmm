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

# pattern for monthly merged satellite rain gauges product
pattern='^.*3B43\\.\\d{8}\\.7A\\.(nc|hdf)'
str_match(pattern='^.*(3B43\\.)(\\d{8})(\\.7A\\.(nc|hdf))', string=string)[,3]

# 3 products to be dowloaded from Mirador (subset possible on request, otherwise download is also possible via ftp)
# 3B42 Daily
    # '3B42_daily.2013.12.29.7.SUB.nc' (bin|hdf)
    # 3B42_daily.2013.12.29.7.nc (bin|hdf)
pattern = '^.*3B42_daily\\.\\d{4}\\.\\d{2}\\.\\d{2}\\.7(\\.SUB|?)\\.(?i)(nc|bin|hdf)'
# 3B42 (which is the 3 hourly)
    # 3B42.20060228.15.7A.HDF
pattern = '^.*3B42\\.\\d{8}\\.\\d{2}\\.7A(\\.SUB|?)\\.(?i)(nc|hdf)'
# 3B43 (TRMM combined monthly)
    # 3B43.20020601.7A.nc (hdf)?
    # 3B43.20020601.7A.SUB.nc ??
pattern = '^.*3B43\\.\\d{8}\\.7A(\\.SUB|?)\\.(?i)(nc|hdf)'




