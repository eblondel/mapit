#' @name enrich_with_classes
#' @aliases enrich_with_classes
#' @title enrich_with_classes
#' @export
#' @description Enrichs a spatialized statistical dataset with classes
#'
#' @usage enrich_with_classes(sf, classints, variable, maptype, scale_factor)
#' 
#' @return an object from \pkg{sf}
#' 
enrich_with_classes <- function(sf, classints, variable, maptype, scale_factor = 1.5){
  outsf <- sf
  outsf$maptype <- maptype
  levels <- as.matrix(attr(classInt::classIntervals2shingle(classints),"levels"))
  switch(maptype,
         "graduated_linear_symbols" = {
           outsf$CLASS <- 0
           for(i in 1:nrow(levels)){
             lev <- levels[[i]]
             if(i==1){
               outsf[outsf[,variable]>=lev[1] & outsf[,variable] <= lev[2],]$CLASS <- i / scale_factor
             }else{
               outsf[outsf[,variable]>lev[1] & outsf[,variable] <= lev[2],]$CLASS <- i / scale_factor
             }
           }
         },
         "graduated_mean_symbols" = {
           outsf$CLASS <- 0
           mids <- sapply(1:nrow(levels), function(i){
             lev <- levels[[i]]
             mid <- lev[1] + (lev[2] - lev[1])/2
             return(mid)
           })
           mid_ratio <- max(mids)/(dim(levels)[1])
           for(i in 1:length(mids)){
             lev <- levels[[i]]
             mid_class <- mids[i] / mid_ratio
             if(i==1){
               outsf[outsf[,variable]>=lev[1] & outsf[,variable] <= lev[2],]$CLASS <- mid_class / scale_factor
             }else{
               outsf[outsf[,variable]>lev[1] & outsf[,variable] <= lev[2],]$CLASS <- mid_class / scale_factor
             }
           }
         }
  )
  return(outsf)
}