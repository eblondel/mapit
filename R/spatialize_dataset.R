#' @name spatialize_dataset
#' @aliases spatialize_dataset
#' @title spatialize_dataset
#' @export
#' @description Spatializes a statistical dataset
#'
#' @usage spatialize_dataset(sfby, sfby.code, stats, by, variable, maptype, m49_codes_to_hide)
#' 
#' @return an object from \pkg{sf}
#' 
spatialize_dataset <- function(sfby = "countries", sfby.code = "M49", stats, by, variable, maptype = "choropleth", m49_codes_to_hide = "010"){
  
  layers <- get_baselayers()
  sf <- layers[[sfby]]
  sf$surface <- sf::st_area(sf, by_element = TRUE)
  if(sfby == "countries") sf <- sf[!sf$M49 %in% m49_codes_to_hide,]
  newdata <- merge(sf, stats, by.x = sfby.code, by.y = by, all.x = TRUE, all.y = FALSE)
  newdata <- newdata[order(newdata$rowid),]
  sfcodes <- unique(newdata[[sfby.code]])
  sfcodes <- sfcodes[!is.na(sfcodes)]
  #in case we use symbols we need to set proper NAs for all smaller polygons
  if(regexpr("symbols", maptype)>0){
    for(sfcode in sfcodes){
      dat = newdata[!is.na(newdata[[sfby.code]]) & newdata[[sfby.code]] == sfcode,]
      idx <- dat[dat$surface == max(dat$surface), ][["rowid"]]
      idx = idx[1]
      if(nrow(newdata[!is.na(newdata[[sfby.code]]) & newdata$rowid != idx & newdata[[sfby.code]] == sfcode,])>0){
        newdata[!is.na(newdata[[sfby.code]]) & newdata$rowid != idx & newdata[[sfby.code]] == sfcode,][[variable]] <- NA
      }
    }
  }
  sf <- newdata
  if(regexpr("symbols", maptype)>0) sf <- sf[!is.na(sf[[variable]]),]
  
  #Specific country cases
  if(sfby == "countries"){
    #provinces of China
    #Aksai Chin
    if(any(!is.na(sf$ROMNAM) & sf$ROMNAM == "Aksai Chin")) sf[!is.na(sf$ROMNAM) & sf$ROMNAM == "Aksai Chin",][[variable]] <- newdata[newdata$M49 == "156" & newdata$ISO3CD == "CHN",][[variable]]
    if(any(!is.na(sf$ROMNAM) & sf$ROMNAM == "Taiwan")) sf[!is.na(sf$ROMNAM) & sf$ROMNAM == "Taiwan",][[variable]] <- newdata[newdata$M49 == "156" & newdata$ISO3CD == "CHN",][[variable]]
    if(any(!is.na(sf$ROMNAM) & sf$ROMNAM == "Hong Kong")) sf[!is.na(sf$ROMNAM) & sf$ROMNAM == "Hong Kong",][[variable]] <- newdata[newdata$M49 == "156" & newdata$ISO3CD == "CHN",][[variable]]
    if(any(!is.na(sf$ROMNAM) & sf$ROMNAM == "Macao")) sf[!is.na(sf$ROMNAM) & sf$ROMNAM == "Macao",][[variable]] <- newdata[newdata$M49 == "156" & newdata$ISO3CD == "CHN",][[variable]]
    
    #Arunachal Pradesh
    if(any(!is.na(sf$ROMNAM) & sf$ROMNAM == "Arunachal Pradesh")) sf[!is.na(sf$ROMNAM) & sf$ROMNAM == "Arunachal Pradesh",][[variable]] <- newdata[newdata$M49 == "356" & newdata$ISO3CD == "IND",][[variable]]
  }
  
  return(sf)
}