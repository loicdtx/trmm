\name{trmm2date}
\alias{trmm2date}
\title{Date parser for TRMM file names}
\usage{
trmm2date(x)
}
\arguments{
  \item{x}{Character or list of characters. A typical TRMM
  file name(s)}
}
\value{
A date object
}
\description{
Date parser for TRMM file names
}
\details{
Works for products downloaded using the
\href{http://mirador.gsfc.nasa.gov/}{mirador} interface or
directly from ftp (The three products supported by this
function are 3B42, 3B42 daily, and 3B43)
}
\author{
Loic Dutrieux
}

