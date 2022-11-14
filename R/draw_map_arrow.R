#' @name draw_map_arrow
#' @aliases draw_map_arrow
#' @title draw_map_arrow
#' @export
#' @description Draws a map arrow
#'
draw_map_arrow <- function(from, to, value, trunc = 15, col = "#0088A2"){

  from.wgs84 = sf::st_transform(from, 4326)
  to.wgs84 = sf::st_transform(to, 4326)
  
  line <- geosphere::gcIntermediate(as.numeric(sf::st_coordinates(from.wgs84)), as.numeric(sf::st_coordinates(to.wgs84)), n=50, addStartEnd = TRUE)
  print(line)
  print(1:(nrow(line)-trunc))
  linestring <- sf::st_linestring(line[1:(dim(line)[1]-trunc),])
  linesf <- sf::st_sf(geom = sf::st_sfc(linestring,crs = 4326))
  linesf <- sf::st_transform(linesf, "+proj=eck4")
  coords = sf::st_coordinates(linesf)

  plot(linesf, add = TRUE, lwd = value, col = col)
  shape::Arrows(coords[nrow(coords)-1,1], coords[nrow(coords)-1,2], coords[nrow(coords),1], coords[nrow(coords),2], arr.length = 0.6, code = 2, 
                arr.type = "triangle", arr.col = col, lcol = col, xpd = TRUE, arr.width = 0.4)
}