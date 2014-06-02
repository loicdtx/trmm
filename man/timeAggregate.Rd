\name{timeAggregate}
\alias{timeAggregate}
\title{Aggregation function for TRMM data}
\usage{
timeAggregate(x, dates, by, FUN = sum, ...)
}
\arguments{
  \item{x}{filename, rasterStack, rasterBrick, or list of
  rasterLayers (filenames)}

  \item{dates}{A date vector (See \code{\link{trmm2date}}
  to extract dates from trmm filenames)}

  \item{by}{Numeric. The lenght of the aggregation period}

  \item{FUN}{The compositing function}

  \item{...}{Arguments to be passed to
  \code{\link{writeRaster}}}
}
\value{
RasterStack with time written to the z dimention
}
\description{
Aggregate daily TRMM data over time, using a defined
function. Also see \code{\link{trmmCompose}} which does
something different.
}
\author{
Loic Dutrieux
}

