#' @name create_map
#' @aliases create_map
#' @title create_map
#' @export
#' @description Creates a map
#'
create_map <- function(sf = NULL, sfby = NULL, sfby.code = NULL,
                       stats = NULL, by = NULL, variable, digits = 2, lang = "en",
                       maptype = "choropleth", classtype = "jenks", classnumber = 5,  breaks,
                       col = "#08519C", pal = NULL, invertpal = FALSE,
                       bgCol = "transparent", bgBorderCol = "transparent", naCol = "gray",naColBox= "lightgray", boundCol = "white", contCol = "lightgray", hashCol= "lightgray",
                       m49_codes_to_hide = "010",
                       add_small_features_as_dots = TRUE, add_small_NA_features_as_dots = FALSE, small_features_dots_cex = 0.4,
                       pch = 21, level.min = NULL, level.max = NULL, level.factor = 1, level.unit = "chars", plot.handler = NULL,
                       legend = TRUE, legendtitle = "Legend", legendunit = "", legendcol = "black", legendpch = pch, legendcex = 0.8, legendpchcol = col, legend_nesting = FALSE, 
                       add_disclaimers = TRUE,
                       add_copyright = TRUE,
                       add = FALSE,
                       debug = FALSE,
                       family = "FuturaStd"){
  
  #get current factor of compression for the plot region (depending on the margin)
  margin_comp_factor = par("plt")[4]-par("plt")[3]
  
  showtext::showtext_auto()
  par(family =  "Arial Unicode MS")
  
  #get layers
  layers <- get_baselayers()
  
  #process and spatialize statistics
  if(!is.null(sfby)) sfby.code <- switch(sfby,
    "countries" = "M49",
    "fao_areas" = "F_CODE",
    "fao_areas_inland" = "F_AREA_INL",
    NULL
  )
  sf <- spatialize_dataset(
    sf = sf, sfby = sfby, sfby.code = sfby.code, 
    stats = stats, by = by, variable = variable, 
    maptype = maptype, 
    m49_codes_to_hide = m49_codes_to_hide
  )
  
  #background
  bg_layer = NULL
  if(!add){
    coords <- data.frame(matrix(c(-180,-90,-180,90,180,90,180,-90,-180,-90),ncol=2, byrow=TRUE))
    colnames(coords) <- c("X","Y")
    each <- 0.1
    newcoords <- do.call("rbind", lapply(1:(nrow(coords) - 1), function(i) {
      i_coords <- coords[i:(i + 1), ]
      out_coords <- data.frame(
        x = seq(from = i_coords[1L,1L], to = i_coords[2L, 1L], by = ifelse(i_coords[1L,1L] <= i_coords[2L, 1L], each, -each)), 
        y = seq(from = i_coords[1L,2L], to = i_coords[2L, 2L], by = ifelse(i_coords[1L,2L] <= i_coords[2L, 2L], each, -each))
      )
      if (i < (nrow(coords) - 1)) out_coords <- out_coords[, -nrow(out_coords)]
      out_coords <- as.matrix(out_coords)
      return(out_coords)
    }))
    
    b_poly <- sf::st_sf(geom = sf::st_sfc(sf::st_polygon(list(newcoords))), crs = 4326)
    b_poly <- sf::st_transform(b_poly, "+proj=eck4")
    bg_layer <- b_poly
    plot(b_poly[1], col = bgCol, border = bgBorderCol, reset = FALSE)
  }
  
  #base continent
  if(!add){
    continent_layer <- "continent"
    if(!is.null(sfby)) if(sfby == "countries") continent_layer = "continent_nopole"
    plot(layers[[continent_layer]][1], col = contCol, border = boundCol, lwd = 0.2, add = TRUE)
  }
  
  #intervals
  # defining INTERVALS
  classColours <- NULL
  classints <- NULL
  if (classtype %in% c("equal","pretty","quantile","fisher","jenks")){
    
    if(classnumber < 2)  stop("The number of class must be greater than 1")
    classints<-classInt::classIntervals(as.numeric(sf[[variable]]),n = classnumber,style = classtype, dataPrecision = digits)
    
    if(!is.null(pal)){
      if(is.function(pal)) pal = pal(classnumber)
      if(invertpal) pal <- rev(pal)
      classColours<-classInt::findColours(classints,pal = pal)
      classColours[is.na(classColours)]<-naCol # added otherwise is transparent!
    }
    
    #enrich with class
    sf <- enrich_with_classes(sf, classints, variable, maptype, 
                              level.min = level.min, level.max = level.max, 
                              level.factor = level.factor,
                              level.unit = level.unit)
    
  } else if (classtype == "fixed"){
    if (missing(breaks)){
      stop("The parameter [breaks] is not specified")
    }else{
      classnumber = length(breaks)
      classints<-classInt::classIntervals(as.numeric(sf[[variable]]),style = "fixed",n = classnumber, fixedBreaks=breaks, dataPrecision=digits)
      
      if(!is.null(pal)){
        if(is.function(pal)) pal = pal(classnumber)
        if(invertpal) pal <- rev(pal)
        classColours<-classInt::findColours(classints,pal = pal)
        classColours[is.na(classColours)]<-naCol # added otherwise is transparent!
      }
      
      #enrich with class
      sf <- enrich_with_classes(sf, classints, variable, maptype, level.min = level.min, level.max = level.max, level.factor = level.factor, level.unit = level.unit)
    }                                                                        
  }    
  
  #
  
  #statistics
  if(maptype == "choropleth"){
    print(class(classColours))
    sf$colour <- classColours
    plot(sf[1], lty=0, bg=bgCol, border="transparent", col=classColours, add = TRUE)
    plot(sf[!is.na(sf$ROMNAM) & sf$ROMNAM == "Aksai Chin", ][1], lty=1, border = hashCol, col=hashCol, lwd=0.1, density=50,add=TRUE)
    if(add_small_features_as_dots){
      small.sf <- sf[sf$Shape_STAr < 0.8 & !is.na(sf$MAPLAB),]
      if(!add_small_NA_features_as_dots){
        small.sf <- small.sf[!is.na(small.sf[[variable]]),]
      }
      plot(sf::st_point_on_surface(small.sf)[1], border="transparent", pch = 19, cex = small_features_dots_cex, col = small.sf$colour, add = TRUE)
    }
  }
  
  #add UN boundaries
  if(!is.null(sfby)) if(sfby == "countries") if(!add){
    boundaries = layers$boundaries
    plot(boundaries[boundaries$TYPE == 1,][1], lwd = 0.45, col = boundCol, lty = "812121", add = TRUE)
    plot(boundaries[boundaries$TYPE == 2,][1], lwd = 0.35, col = boundCol, lty = "21", add = TRUE)
    plot(boundaries[boundaries$TYPE == 3,][1], lwd = 0.35, col = boundCol, lty = "21", add = TRUE)
    plot(boundaries[boundaries$TYPE == 4,][1], lwd = 0.2, col = boundCol, lty = "11",  add = TRUE)
  }
  
  #other maptypes to be displayed after UN boundaries
  if(startsWith(maptype, "graduated")){
    if(!is.null(sfby)) if(sfby == "fao_areas"){
      fao_areas_lines <- layers$fao_areas_lines
      plot(fao_areas_lines, bg=bgCol, col="blue", add = TRUE)
    }
    sf_points <- sf::st_point_on_surface(sf)
    if(!is.null(plot.handler)){
      #with embedded plot
      for(i in 1:nrow(sf_points)){
        sf_plot_obj <- sf_points[i,]
        sf_plot_obj_coords <- as.numeric(sf::st_coordinates(sf_plot_obj))
        subplot.size <- c(
          graphics::grconvertX(sf_plot_obj$CLASS, "chars", "inches"),
          graphics::grconvertY(sf_plot_obj$CLASS, "chars", "inches")
        )
        Hmisc::subplot(plot.handler(sf_plot_obj), size = subplot.size, x = sf_plot_obj_coords[1], y = sf_plot_obj_coords[2])
      }
    }else{
      #with simple symbols
      plot(sf_points, lty=1, bg=col, col = col, pch = pch, cex = sf$CLASS*2.6, add = TRUE) 
    }
    if(debug) plot(do.call("rbind", lapply(1:nrow(sf_points),function(i){
      sf::st_buffer(sf_points[i,], dist = (abs(par("usr")[3] * 1 / margin_comp_factor) - abs(graphics::grconvertY(sf_points[i,]$CLASS, "chars", "user")))/2)
    })), lty=1, bg="transparent", col = "transparent", border = "red", add = TRUE)
  }
  
  if(legend) if(!is.null(classints)){
    #TODO legend coordinates elements are +eck4 oriented... need to provided generic solution
    #legend for classes
    classLeg = classints
    print("Data distribution")
    if(classtype == "fixed") if(!missing(digits)){
      classLeg$brks<-round(classLeg$brks,digits)
    } 
    x<-print(classLeg,cutlabels=F,over=">",under="<")
    label = paste(names(x), legendunit)
    labelLength = nchar(label)
    legendX <- -16800000
    legendY <- -4000000
    if(maptype == "choropleth"){
      create_legend(legendX, legendY, fill=attr(classColours,"palette"), cex=0.8, y.intersp=1.5, 
                    legend=label, text.width = labelLength, box.col="transparent", xjust=0, border="transparent", text.col=legendcol,
                    box.factor = 2,
                    family = family, text.font = 1)
      #legend for 'no data'
      naLabel = "No Data"
      naLabelLength = nchar(naLabel)
      legendItemY <- 640000
      create_legend(legendX, legendY - ((length(names(x))+0.33)*legendItemY), fill = naCol, box.factor = 2, cex=0.8, y.intersp=1.5, 
                 legend=naLabel, text.width = naLabelLength * 2, box.col="transparent", xjust=0, border="transparent", text.col=legendcol,
                 family = family, text.font = 1)
    }else if(startsWith(maptype,"graduated")){
      
      classes <- unique(sf$CLASS)
      classes <- classes[order(classes)]
      
      if(legend_nesting){
        
        classes = rev(classes)
        print(classes)
        label = rev(label)
        print(label)
        
        legendX = -16500000
        legendY = -6000000
        crc_x <- legendX
        crc_y <- legendY
       
        base_y = NULL
        max_r_user = NULL
        for(i in 1:length(classes)){
          class = classes[i]
          
          r_user = (abs(par("usr")[3] * 1 / margin_comp_factor) - abs(graphics::grconvertY(class, "chars", "user")))/2
          if(i==1) max_r_user = r_user
          if(i>1){
            r_user = max_r_user * classes[i]/classes[1]
            crc_y = base_y + r_user
          }
          
          crc <- sf::st_sf(sf::st_sfc(sf::st_point(x = c(crc_x, crc_y))))
          sf::st_crs(crc) <- sf::st_crs(bg_layer)
          crc_buffer = sf::st_buffer(crc, dist = r_user)
          if(i==1){
            base_y = sf::st_bbox(crc_buffer)$ymin
          }
          #plot(crc, lty = 1, bg = "transparent", col = legendpchcol, pch = pch, cex = class*1.2, add = TRUE)
          plot(crc_buffer, lty=1, bg="transparent", col = "transparent", border = legendpchcol, add = TRUE)
          top_y = sf::st_bbox(crc_buffer)$ymax
          text(crc_x + max_r_user*1.5, top_y, labels = label[i], cex = legendcex, col = legendpchcol, adj = 0)
          rect(crc_x, top_y, crc_x + max_r_user*1.25, top_y, border = legendpchcol)
        }
      }else{
        legend(legendX, legendY, cex = 0.8, col = legendpchcol, pch = legendpch, pt.cex=classes*2.6, x.intersp=2, y.intersp=2, 
               legend=label, text.width = labelLength * 2, box.col="transparent", xjust=0, border="transparent", text.col=legendcol,
               text.font = 1)
      }
    }
    
    #legend title
    text(legendX+200000, -3750000, legendtitle, adj = c(0,0), font=2, cex=.8, col=legendcol, family = family)
  }

  showtext::showtext_auto(FALSE)
  
}