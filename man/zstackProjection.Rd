% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zstackProjection.R
\name{zstackProjection}
\alias{zstackProjection}
\title{Calculate the projection of a z-stack image}
\usage{
zstackProjection(
  image_data = NULL,
  projection_method = "max",
  as_array = FALSE
)
}
\arguments{
\item{image_data}{An array of EBImage object}

\item{projection_method}{A character stating the method for calculating
the z-stack projection (either "mean" or "max")}

\item{as_array}{A Boolean stating whether the returned data is a array or
of class EBImage. (The default value is FALSE.)}
}
\value{
An array (z-stack projection of a 3D image).
}
\description{
Calculate the projection of a z-stack image
}
