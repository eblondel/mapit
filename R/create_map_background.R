#' @name create_map_background
#' @aliases create_map_background
#' @title create_map_background
#' @export
#' @description Creates a map background
#'
create_map_background <- function(bbox = NULL, bgCol = "transparent", bgBorderCol = "lightgray"){
  coords <- data.frame(matrix(c(-180,-90,-180,90,180,90,180,-90,-180,-90),ncol=2, byrow=TRUE))
  if(!is.null(bbox)) coords <- data.frame(matrix(c(bbox$xmin, bbox$ymin, bbox$xmin, bbox$ymax, bbox$xmax, bbox$ymax, bbox$xmax, bbox$ymin, bbox$xmin, bbox$ymin),ncol=2,byrow=TRUE))
  colnames(coords) <- c("X","Y")
  each <- 2L
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
  plot(b_poly[1], col = bgCol, border = bgBorderCol, reset = FALSE)
  return(b_poly)
}