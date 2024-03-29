% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readCzi.R
\name{readCzi}
\alias{readCzi}
\title{Imports czi files}
\usage{
readCzi(input_file = NULL)
}
\arguments{
\item{input_file}{Path to a CZI file as a string.}
}
\value{
A multidimensional array with the following dimensions:
\code{(x, y, c, z)} with the channels \code{c}.
}
\description{
`readCzi()` returns a multidimensional array with pixel intensities
from 0 to 1. The array's dimensions are: 1) row (y), 2) column (x),
3) channel (red, green, blue), 4) z sections (z).
}
