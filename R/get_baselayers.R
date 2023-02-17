#' @name get_baselayers
#' @aliases get_baselayers
#' @title get_baselayers
#' @export
#' @description Get UN base layers as \pkg{sf} objects based on UN-FAO Fisheries & Aquaculture Division
#'  Geoserver (using the OGC WFS protocol). Once retrieved, the layers will be cached internally to avoid
#'  re-fetching layers each time.
#'
#' @usage get_baselayers(clear)
#' 
#' @return an \code{list} of \pkg{sf} objects
#' 
#' @examples
#' \donttest{
#'   layers = get_baselayers()
#' }
#' 
get_baselayers <- function(clear = FALSE){
  if(clear) .mapit.cache$layers <- NULL
  if(!is.null(.mapit.cache$layers)) return(.mapit.cache$layers)
  invisible(get_baselayer("continent_nopole", "fifao:UN_CONTINENT2_NOPOLE", "+proj=eck4", cache = TRUE))
  invisible(get_baselayer("continent", "fifao:UN_CONTINENT2", "+proj=eck4", cache = TRUE))
  invisible(get_baselayer("countries", "fifao:country_bounds", "+proj=eck4", cache = TRUE))
  invisible(get_baselayer("boundaries", "fifao:UN_intbnd", "+proj=eck4", cache = TRUE))
  invisible(get_baselayer("fao_areas", "fifao:FAO_AREAS_CWP", "+proj=eck4", cache = TRUE))
  invisible(get_baselayer("fao_areas_inland", "fifao:FAO_AREAS_INLAND", "+proj=eck4", cache = TRUE))
  invisible(get_baselayer("fao_areas_lines", "fifao:FAO_MAJOR_Lines_ERASE", "+proj=eck4", cache = TRUE))
  return(.mapit.cache$layers)
}

#' @name get_baselayer
#' @aliases get_baselayer
#' @title get_baselayer
#' @export
#' @description Get UN base layer as \pkg{sf} object based on UN-FAO Fisheries & Aquaculture Division
#'  Geoserver (using the OGC WFS protocol).
#'
#' @usage get_baselayer(id, layer, crs, cache)
#' 
#' @return an \code{list} of \pkg{sf} objects
#' 
#' @examples
#' \donttest{
#'   get_baselayer("continent", "fifao:UN_CONTINENT2_NOPOLE")
#' }
#' 
get_baselayer <- function(id, layer, crs = NULL, cache = TRUE){
  
  if(cache){
    if(is.null(.mapit.cache$layers)) .mapit.cache$layers <- list()
  }
  
  #ows4R connector
  WFS_UNFAO_NFI <- ows4R::WFSClient$new(
    url = "https://www.fao.org/fishery/geoserver/wfs",
    serviceVersion = "1.0.0"
  )
  
  out <- WFS_UNFAO_NFI$getFeatures(layer)
  sf::st_crs(out) <- 4326
  out$rowid <- 1:nrow(out)
  if(!is.null(crs)) if(crs != 4326){
    out <- sf::st_transform(out, crs = crs)
  }
  
  if(cache) .mapit.cache$layers[[id]] <- out
  
  return(out)
}
