\name{warpTrmm2Raster}
\alias{warpTrmm2Raster}
\title{Reprojects a dataset to the projection of another one}
\usage{
warpTrmm2Raster(target, trmm, filename, buffer = 1e+05, multiple = TRUE,
  run = TRUE)
}
\arguments{
  \item{target}{Raster* object.}

  \item{trmm}{Raster object written to file and that should
  be re-projected to the target crs.}

  \item{filename}{Character. Output filename (must be a
  gdal supported format)}

  \item{buffer}{Numeric. Padding around the target extent,
  in crs units}

  \item{multiple}{Logical. Should the output resolution be
  a multiple of the target's resolution?}

  \item{run}{Logical. SHould the command be ran (gdalwarp
  via system call). Otherwise it just creates the gdalwarp
  string.}
}
\value{
Object of class spaceTime or character
}
\description{
Reprojects a dataset to the projection of another one
allowing the user to set a buffer and pixel alignment (even
if resolution is different)
}

