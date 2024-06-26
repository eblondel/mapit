#' @name enrich_with_classes
#' @aliases enrich_with_classes
#' @title enrich_with_classes
#' @export
#' @description Enrichs a spatialized statistical dataset with classes
#'
#' @usage enrich_with_classes(sf, classints, variable, maptype, level.min, level.max, level.unit)
#' 
#' @return an object from \pkg{sf}
#' 
enrich_with_classes <- function(sf, classints, variable, maptype, level.min = NULL, level.max = NULL, level.factor = 1, level.unit = "chars"){

  
  outsf <- sf
  outsf$maptype <- maptype
  levels <- attr(classInt::classIntervals2shingle(classints),"levels")
  if(is.null(level.min)) level.min = 1
  if(is.null(level.max)) level.max = length(levels)
  
  if(level.unit == "inches"){
    level.min = graphics::grconvertX(level.min, "inches", "chars")
    level.max = graphics::grconvertX(level.max, "inches", "chars")
  }
  
  switch(maptype,
         "graduated_linear_symbols" = {
           level.seq = seq(level.min, level.max, by = (level.max-level.min)/(length(levels)-1))
           outsf$CLASS <- 0
           print(levels)
           for(i in 1:length(levels)){
             lev <- levels[[i]]
             print(outsf[[variable]])
             if(i==1){
               if(any(outsf[[variable]]>=lev[1] & outsf[[variable]] <= lev[2])) outsf[outsf[[variable]]>=lev[1] & outsf[[variable]] <= lev[2],]$CLASS <- level.seq[i]
             }else{
               if(any(outsf[[variable]]>lev[1] & outsf[[variable]] <= lev[2])) outsf[outsf[[variable]]>lev[1] & outsf[[variable]] <= lev[2],]$CLASS <- level.seq[i]
             }
           }
           print("ended")
         },
         "graduated_mean_symbols" = {
           outsf$CLASS <- 0
           mids <- sapply(1:length(levels), function(i){
             lev <- levels[[i]]
             mid <- lev[1] + (lev[2] - lev[1])/2
             return(mid)
           })
           level.seq <- level.min + (level.max-level.min) * (mids[1:length(levels)]-mids[1])/(mids[length(levels)]-mids[1])
           for(i in 1:length(mids)){
             lev <- levels[[i]]
             if(i==1){
               if(any(outsf[[variable]]>=lev[1] & outsf[[variable]] <= lev[2])) outsf[outsf[[variable]]>=lev[1] & outsf[[variable]] <= lev[2],]$CLASS <- level.seq[i]
             }else{
               if(any(outsf[[variable]]>lev[1] & outsf[[variable]] <= lev[2])) outsf[outsf[[variable]]>lev[1] & outsf[[variable]] <= lev[2],]$CLASS <- level.seq[i]
             }
           }
         }
  )
  if("CLASS" %in% colnames(outsf)){
    outsf$CLASS = outsf$CLASS * level.factor
  }
  return(outsf)
}