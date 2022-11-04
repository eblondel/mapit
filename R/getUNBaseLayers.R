#' @name getUNBaseLayers
#' @aliases getUNBaseLayers
#' @title getUNBaseLayers
#' @export
#' @description Get UN base layers as \pkg{sf} objects based on UN-FAO Fisheries & Aquaculture Division
#'  Geoserver (using the OGC WFS protocol). Once retrieved, the layers will be cached internally to avoid
#'  re-fetching layers each time.
#'
#' @usage getUNBaseLayers()
#' 
#' @return an \code{list} of \pkg{sf} objects
#' 
#' @examples
#' \donttest{
#'   layers = getUNBaseLayers()
#' }
#' 
getUNBaseLayers <- function(){
  
  if(!is.null(.mapit.cache$layers)) return(.mapit.cache$layers)
  
  #ows4R connector
  WFS_UNFAO_NFI <- ows4R::WFSClient$new(
    url = "https://www.fao.org/fishery/geoserver/wfs",
    serviceVersion = "1.0.0",
    logger = "DEBUG"
  )
  
  #continent
  continent.sf <- WFS_UNFAO_NFI$getFeatures("fifao:UN_CONTINENT2_NOPOLE")
  sf::st_crs(continent.sf) <- 4326
  continent.sf <- sf::st_transform(continent.sf, "+proj=eck4")
  
  #UN country members (polygon)
  countries.sf <- WFS_UNFAO_NFI$getFeatures("fifao:country_bounds")
  sf::st_crs(countries.sf) <- 4326
  countries.sf <- sf::st_transform(countries.sf, "+proj=eck4")
  countries.sf$rowid <- 1:nrow(countries.sf)

  #UN country members (lines)
  boundaries.sf <- WFS_UNFAO_NFI$getFeatures("fifao:UN_intbnd")
  sf::st_crs(boundaries.sf) <- 4326
  boundaries.sf <- sf::st_transform(boundaries.sf, "+proj=eck4")

  
  layers<-list(
    continent = continent.sf,
    countries = countries.sf,
    boundaries = boundaries.sf
  )
  .mapit.cache$layers <- layers
  return(layers)
}
