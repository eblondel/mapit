#' @name create_flow_map
#' @aliases create_flow_map
#' @title create_flow_map
#' @export
#' @description Creates a flow map
#'
create_flow_map <- function(sf, by, data, data.col.from, data.col.to, 
                            arrow.trunc = 20, arrow.col = "#C3E4E5",
                            target.highlight = TRUE, target.col = "#0088A2",
                            label.box.col = "#0088A2", label.bg = "#C3E4E5"){
  
  if(!is(data,"data.frame")) stop("Data should of class 'data.frame'")

  max.cex <- 10
  cex.ratio <- max.cex / max(data$value)
  
  for(i in 1:nrow(data)){
    
    if(data[i,][,data.col.from] == data[i,][,data.col.to]){
      if(target.highlight) plot(sf[sf[[by]] %in% data[i,][,data.col.to],][1], col = target.col, border = "white", add = TRUE)
      next
    }
    
    sf.from <- sf::st_centroid(sf[sf[[by]] == data[i,][,data.col.from],])
    sf.to <- sf::st_centroid(sf[sf[[by]] == data[i,][,data.col.to],])
    sf.to.pt <- sf::st_point_on_surface(sf[sf[[by]] == data[i,][,data.col.to],])
    draw_map_arrow(
      from = sf.from,
      to = sf.to,
      value = ceiling(data[i,]$value * cex.ratio),
      trunc = arrow.trunc,
      col = arrow.col
    )
    sf.from.coords <- sf::st_coordinates(sf.from)
    mapit::create_legend(sf.from.coords[1], sf.from.coords[2], paste0(data[i,]$value*100,"%"), text.col = "white", text.font = 2, 
                         box.col = label.box.col, bg = label.bg, xjust = 0.5, yjust = 0.5, x.intersp = 0)
    
  }
  for(i in 1:nrow(data)){
    if(data[i,][,data.col.from] == data[i,][,data.col.to]){
      sf.to <- sf::st_centroid(sf[sf[[by]] == data[i,][,data.col.to],])
      plot(sf.to.pt, pch = "\u21BA", font = 2, col = "white", cex = 4, add = TRUE)
      
      sf.to.coords <- sf::st_coordinates(sf.to)
      mapit::create_legend(sf.to.coords[1], sf.to.coords[2], paste0(data[i,]$value*100,"%"), text.col = "white", text.font = 2, 
                           box.col = label.box.col, bg = label.bg, xjust = 0.5, yjust = 1, x.intersp = 0)
    }
  }
}
