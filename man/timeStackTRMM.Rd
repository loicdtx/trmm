\name{timeStackTRMM}
\alias{timeStackTRMM}
\title{Wrapper to create TRMM stack with time dimension}
\usage{
timeStackTRMM(x, pattern = NULL, ...)
}
\arguments{
  \item{x}{character. dir containing the files to be
  stacked or character list (the files). IN the former
  case, it is recommended to use the \code{pattern}
  argument}

  \item{pattern}{See \code{\link{list.files}}}

  \item{...}{Arguments to be passed to \link{writeRaster}.
  If specifying a filename, it is strongly recommended to
  also set a datatype.}
}
\description{
Wrapper to create TRMM stack with time dimension
}
\details{
At the moment this function will only work for single layer
datasets (there is a subset option in mirador in order to
get only the desired layer)
}
\author{
Loic Dutrieux
}

