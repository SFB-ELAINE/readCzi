% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readCziMetadata.R
\name{readCziMetadata}
\alias{readCziMetadata}
\alias{readcziMetadata}
\alias{readCzimetadata}
\alias{readCZIMetadata}
\title{readCziMetadata}
\usage{
readCziMetadata(input_file = NULL)
}
\arguments{
\item{input_file}{A character (path to czi file to be read)}
}
\value{
A data frame with key information from the metadata is returned.
}
\description{
Reads and saves key values from czi metadata as well as
entire metadata as txt
}
\details{
This functions saves the metadata of a czi file as txt and also
extract the most important information and saves it in a data frame.
}
\author{
Kai Budde
}
