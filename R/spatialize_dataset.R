#' @name spatialize_dataset
#' @aliases spatialize_dataset
#' @title spatialize_dataset
#' @export
#' @description Spatializes a statistical dataset
#'
#' @usage spatialize_dataset(sf, sfby, sfby.code, stats, by, variable, maptype, m49_codes_to_hide)
#' 
#' @return an object from \pkg{sf}
#' 
spatialize_dataset <- function(sf = NULL, sfby = NULL, sfby.code = NULL, stats, by, variable, maptype = "choropleth", m49_codes_to_hide = "010"){
  
  if(is.null(sfby.code)) stop("Argument 'sfby.code' is required!")
  

  if(!is.null(sfby)){
    sf <- switch(sfby,
                 "countries" = mapit::un_countries_eck4,
                 "countries_lowres" = mapit::un_countries_lowres_eck4,
                 "fao_areas" = mapit::fao_areas_lowres_eck4,
                 "fao_major_areas" = mapit::fao_major_areas_lowres_eck4,
                 "fao_areas_inland" = mapit::fao_areas_inland_eck4,
                 "un_sdg_regions" = mapit::un_sdg_regions_lowres_eck4,
                 "un_sdg_regions_placemarks" = mapit::un_sdg_regions_placemarks_eck4,           
    )
    if(startsWith(sfby, "countries")){
      sf <- rbind(
        sf[is.na(sf$M49),],
        sf[!is.na(sf$M49) & !sf$M49 %in% m49_codes_to_hide,]
      )
    }
  }
  sf$surface <- sf::st_area(sf, by_element = TRUE)
  newdata <- merge(sf, stats, by.x = sfby.code, by.y = by, all.x = TRUE, all.y = FALSE)
  newdata <- newdata[order(newdata$rowid),]
  sfcodes <- unique(newdata[[sfby.code]])
  sfcodes <- sfcodes[!is.na(sfcodes)]
  #in case we use symbols we need to set proper NAs for all smaller polygons
  if(regexpr("symbols", maptype)>0){
    for(sfcode in sfcodes){
      dat = newdata[!is.na(newdata[[sfby.code]]) & newdata[[sfby.code]] == sfcode,]
      
      #exceptions
      if(!is.null(sfby)) if(sfby == "countries_lowres") if(sfby.code == "M49"){
        #Malaysia
        if(sfcode == 458){
          newdata[!is.na(newdata[[sfby.code]]) & newdata$rowid != 1427 & newdata[[sfby.code]] == sfcode,][[variable]] <- NA
          next
        }
      }
      
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
  if(!is.null(sfby)){
    
    switch(sfby,
      "countries" = {
        #provinces of China
        ref_value = newdata[newdata$M49 == "156" & newdata$ISO3CD == "CHN",][[variable]]
        ref_value = ref_value[!is.na(ref_value)][1]
        if(any(!is.na(sf$ROMNAM) & sf$ROMNAM == "Aksai Chin")) sf[!is.na(sf$ROMNAM) & sf$ROMNAM == "Aksai Chin",][[variable]] <- ref_value
        if(any(!is.na(sf$ROMNAM) & sf$ROMNAM == "Taiwan")) sf[!is.na(sf$ROMNAM) & sf$ROMNAM == "Taiwan",][[variable]] <- ref_value
        if(any(!is.na(sf$ROMNAM) & sf$ROMNAM == "Hong Kong")) sf[!is.na(sf$ROMNAM) & sf$ROMNAM == "Hong Kong",][[variable]] <- ref_value
        if(any(!is.na(sf$ROMNAM) & sf$ROMNAM == "Macao")) sf[!is.na(sf$ROMNAM) & sf$ROMNAM == "Macao",][[variable]] <- ref_value
        
        #Arunachal Pradesh
        ref_value2 = newdata[newdata$M49 == "356" & newdata$ISO3CD == "IND",][[variable]]
        ref_value2 = ref_value2[!is.na(ref_value2)][1]
        if(any(!is.na(sf$ROMNAM) & sf$ROMNAM == "Arunachal Pradesh")) sf[!is.na(sf$ROMNAM) & sf$ROMNAM == "Arunachal Pradesh",][[variable]] <- ref_value2
      },
      "countries_lowres" = {
        #provinces of China
        #Aksai Chin
        ref_value = newdata[newdata$M49 == "156" & newdata$ISO_3 == "CHN",][[variable]]
        ref_value = ref_value[!is.na(ref_value)][1]
        if(any(!is.na(sf$Terr_Name) & sf$Terr_Name == "Aksai Chin")) sf[!is.na(sf$Terr_Name) & sf$Terr_Name == "Aksai Chin",][[variable]] <- ref_value
        if(any(!is.na(sf$Terr_Name) & sf$Terr_Name == "Taiwan")) sf[!is.na(sf$Terr_Name) & sf$Terr_Name == "Taiwan",][[variable]] <- ref_value
        if(any(!is.na(sf$Terr_Name) & sf$Terr_Name == "Hong Kong")) sf[!is.na(sf$Terr_Name) & sf$Terr_Name == "Hong Kong",][[variable]] <- ref_value
        if(any(!is.na(sf$Terr_Name) & sf$Terr_Name == "Macao")) sf[!is.na(sf$Terr_Name) & sf$Terr_Name == "Macao",][[variable]] <- ref_value
        
        #Arunachal Pradesh
        ref_value2 = newdata[newdata$M49 == "356" & newdata$ISO_3 == "IND",][[variable]]
        ref_value2 = ref_value2[!is.na(ref_value2)][1]
        if(any(!is.na(sf$Terr_Name) & sf$Terr_Name == "Arunashal Pradesh")) sf[!is.na(sf$Terr_Name) & sf$Terr_Name == "Arunashal Pradesh",][[variable]] <- ref_value2
      }
    )
  }
    
  return(sf)
}
