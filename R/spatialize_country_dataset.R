#' @name spatialize_country_dataset
#' @aliases spatialize_country_dataset
#' @title spatialize_country_dataset
#' @export
#' @description Spatializes a statistical country dataset
#'
#' @usage spatialize_country_dataset(stats, by, variable, maptype, m49_codes_to_hide)
#' 
#' @return an object from \pkg{sf}
#' 
spatialize_country_dataset <- function(stats, by, variable, maptype = "choropleth", m49_codes_to_hide = "010"){
  
  layers <- get_baselayers()
  sf <- layers$countries
  sf$surface <- sf::st_area(sf, by_element = TRUE)
  sf <- sf[!sf$M49 %in% m49_codes_to_hide,]
  newdata <- merge(sf, stats, by.x = "M49", by.y = by, all.x = TRUE, all.y = FALSE)
  newdata <- newdata[order(newdata$rowid),]
  countries <- unique(newdata$M49)
  countries <- countries[!is.na(countries)]
  #in case we use symbols we need to set proper NAs for all country smaller polygons
  if(regexpr("symbols", maptype)>0){
    for(country in countries){
      dat = newdata[!is.na(newdata$M49) & newdata$M49 == country,]
      idx <- dat[dat$surface == max(dat$surface), ][["rowid"]]
      idx = idx[1]
      if(nrow(newdata[!is.na(newdata$M49) & newdata$rowid != idx & newdata$M49 == country,])>0){
        newdata[!is.na(newdata$M49) & newdata$rowid != idx & newdata$M49 == country,][[variable]] <- NA
      }
    }
  }
  sf <- newdata
  if(regexpr("symbols", maptype)>0) sf <- sf[!is.na(sf[[variable]]),]
  
  #provinces of China
  #Aksai Chin
  if(any(!is.na(sf$ROMNAM) & sf$ROMNAM == "Aksai Chin")) sf[!is.na(sf$ROMNAM) & sf$ROMNAM == "Aksai Chin",][[variable]] <- newdata[newdata$M49 == "156" & newdata$ISO3CD == "CHN",][[variable]]
  if(any(!is.na(sf$ROMNAM) & sf$ROMNAM == "Taiwan")) sf[!is.na(sf$ROMNAM) & sf$ROMNAM == "Taiwan",][[variable]] <- newdata[newdata$M49 == "156" & newdata$ISO3CD == "CHN",][[variable]]
  if(any(!is.na(sf$ROMNAM) & sf$ROMNAM == "Hong Kong")) sf[!is.na(sf$ROMNAM) & sf$ROMNAM == "Hong Kong",][[variable]] <- newdata[newdata$M49 == "156" & newdata$ISO3CD == "CHN",][[variable]]
  if(any(!is.na(sf$ROMNAM) & sf$ROMNAM == "Macao")) sf[!is.na(sf$ROMNAM) & sf$ROMNAM == "Macao",][[variable]] <- newdata[newdata$M49 == "156" & newdata$ISO3CD == "CHN",][[variable]]
  
  #Arunachal Pradesh
  if(any(!is.na(sf$ROMNAM) & sf$ROMNAM == "Arunachal Pradesh")) sf[!is.na(sf$ROMNAM) & sf$ROMNAM == "Arunachal Pradesh",][[variable]] <- newdata[newdata$M49 == "356" & newdata$ISO3CD == "IND",][[variable]]
  
  return(sf)
}