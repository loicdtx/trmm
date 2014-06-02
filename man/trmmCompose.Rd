\name{trmmCompose}
\alias{trmmCompose}
\title{Compositing function for TRMM data}
\usage{
trmmCompose(x, dates, by, FUN = sum, ...)
}
\arguments{
  \item{x}{filename, rasterStack, rasterBrick, or list of
  rasterLayers (filenames)}

  \item{dates}{A date vector (See \code{\link{trmm2date}}
  to extract dates from trmm filenames)}

  \item{by}{Numeric. The lenght of the composite in days}

  \item{FUN}{The compositing function}

  \item{...}{Arguments to be passed to
  \code{\link{writeRaster}}}
}
\value{
A RasterStack with time written to the z dimension
}
\description{
Creates composites that can be matched with each others
from one year to the other. The same way that MODIS
composites work. It differs from
\code{\link{timeAggregate}} by the fact that aggregated
periods can be matched between years. However, warning
because the last composite of the year is likely to be
truncated (particularly relevant in case of cumulative
value composite, which is the default here)
}
\author{
Loic Dutrieux
}

