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
  invisible(get_baselayer("countries_lowres", "fifao:country_bounds_legacy", "+proj=eck4", cache = TRUE))
  invisible(get_baselayer("boundaries", "fifao:UN_intbnd", "+proj=eck4", cache = TRUE))
  invisible(get_baselayer("boundaries_lowres", "fifao:UN_intbnd_legacy", "+proj=eck4", cache = TRUE))
  invisible(get_baselayer("fao_areas", "fifao:FAO_AREAS_ERASE_LOWRES", "+proj=eck4", cache = TRUE))
  invisible(get_baselayer("fao_areas_inland", "fifao:FAO_AREAS_INLAND", "+proj=eck4", cache = TRUE))
  invisible(get_baselayer("fao_areas_lines", "fifao:FAO_MAJOR_Lines_ERASE", "+proj=eck4", cache = TRUE))
  invisible(get_baselayer("WBYA25", "fifao:WBYA25", "+proj=eck4", cache = TRUE))
  invisible(get_baselayer("un_sdg_regions", "fifao:cl_un_sdg_regions", "+proj=eck4", cache = TRUE))
  invisible(get_baselayer("un_sdg_regions_lowres", "fifao:cl_un_sdg_regions_lowres", "+proj=eck4", cache = TRUE))
  invisible(get_baselayer("un_sdg_regions_placemarks", "fifao:cl_un_sdg_regions_placemarks", "+proj=eck4", cache = TRUE))
  
  #patches for country_bounds_lowres
  countries_lowres = get_baselayers()$countries_lowres
  #patch for Morocco boundaries
  #countries_lowres[!is.na(countries_lowres$M49) & countries_lowres$M49 == 504,]$the_geom = get_baselayers()$countries[!is.na(get_baselayers()$countries$M49) & get_baselayers()$countries$M49 == 504,]$geom
  #patch to recover SPM and SHN (not visible in country_bounds_lowres)
  spm_shn = get_baselayers()$countries[!is.na(get_baselayers()$countries$ISO3CD) & get_baselayers()$countries$ISO3CD %in% c("SPM", "SHN"),]
  spm_shn = spm_shn[,c("ROMNAM", "ISO3CD", "M49")]
  colnames(spm_shn) = c("Terr_Name","ISO_3","M49","the_geom")
  sf::st_geometry(spm_shn) <- "the_geom"
  countries_lowres = sf::st_sf(plyr::rbind.fill(countries_lowres, spm_shn), sf_column_name = "the_geom")
  countries_lowres$rowid <- 1:nrow(countries_lowres)
  .mapit.cache$layers$countries_lowres = countries_lowres
  
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
    if(layer == "fifao:country_bounds_legacy"){
      out <- rbind(
        out[is.na(out$ISO_3),],
        out[!is.na(out$ISO_3) & out$ISO_3 != "EUR",]
      )
    }
  }
  
  if(cache) .mapit.cache$layers[[id]] <- out
  
  return(out)
}
