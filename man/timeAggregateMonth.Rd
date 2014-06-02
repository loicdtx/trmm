\name{timeAggregateMonth}
\alias{timeAggregateMonth}
\title{Aggregation function for TRMM data to monthly data}
\usage{
timeAggregateMonth(x, dates = NULL, FUN = sum, ...)
}
\arguments{
  \item{x}{filename, rasterStack, rasterBrick, or list of
  rasterLayers (filenames)}

  \item{dates}{(optional) A date vector (See
  \code{\link{trmm2date}} to extract dates from trmm
  filenames).  If no date vector is supplied, the function
  will try to extract the time onformation from the z
  dimension of the raster object (if x is a RasterStack or
  RasterBrick)}

  \item{FUN}{The compositing function (sum by default)}

  \item{...}{Arguments to be passed to
  \code{\link{writeRaster}}}
}
\value{
RasterStack with time written to the z dimention
}
\description{
Creates monthly composites from daily trmm data
}
\examples{
data(dailyTRMM)
monthly <- timeAggregateMonth(daily)
}
\author{
Loic Dutrieux
}

