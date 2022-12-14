% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_baselayers.R
\name{get_baselayer}
\alias{get_baselayer}
\title{get_baselayer}
\usage{
get_baselayer(id, layer, crs, cache)
}
\value{
an \code{list} of \pkg{sf} objects
}
\description{
Get UN base layer as \pkg{sf} object based on UN-FAO Fisheries & Aquaculture Division
 Geoserver (using the OGC WFS protocol).
}
\examples{
\donttest{
  get_baselayer("continent", "fifao:UN_CONTINENT2_NOPOLE")
}

}
