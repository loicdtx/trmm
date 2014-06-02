\name{trmm2spi}
\alias{trmm2spi}
\title{Compute spi}
\usage{
trmm2spi(x, scale, kernel = list(type = "rectangular", shift = 0),
  distribution = "Gamma", fit = "ub-pwm", na.rm = FALSE,
  ref.start = NULL, ref.end = NULL, keep = FALSE, params = NULL, ...)
}
\arguments{
  \item{x}{filename, rasterStack, rasterBrick, with time
  written on the z dimension}

  \item{scale}{See \code{\link{spi}}}

  \item{kernel}{See \code{\link{spi}}}

  \item{distribution}{See \code{\link{spi}}}

  \item{na.rm}{See \code{\link{spi}}}

  \item{ref.start}{See \code{\link{spi}}}

  \item{ref.end}{See \code{\link{spi}}}

  \item{keep}{See \code{\link{spi}} (called x in the
  original \code{\link{spi}} function)}

  \item{param}{See \code{\link{spi}}}

  \item{...}{Arguments to be passed to
  \code{\link{writeRaster}}}
}
\value{
RasterStack with time written to the z dimention
}
\description{
Compute spi from a rasterStack/Brick of monthly rainfall
data
}
\examples{
data(dailyTRMM)
monthly <- timeAggregateMonth(daily)
spiSpatial <- trmm2spi(monthly, scale=6)

# Plot pixel number 2 of that brick
plot(zoo(as.vector(spiSpatial[2]), getZ(spiSpatial)))
}
\author{
Loic Dutrieux
}

