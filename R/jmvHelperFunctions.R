jmvSanitizeOption <- function(cvalue = NULL, default = NULL, type.numeric = FALSE, ignore = c("auto", "none"), vrange = NULL) {
  
  result <- default
  
  if (!is.null(cvalue)) {
    if (nchar(cvalue) >0) {
      if(type.numeric) {
        if (!is.na(as.numeric(cvalue))) {
          result <- as.numeric(cvalue)
          if (!is.null(vrange)) {
            if (result < vrange[1] | result > vrange[2]) {
              result <- default
            }
          }
        }
      } else {
        if(!is.na(cvalue)) {
          if(cvalue %in% ignore) {
            
          } else {
            result <- cvalue
          }
        }
      }
    }
  }
  return(result)
}

jmvClearPlotBackground <- function(myplot) {
  return(myplot +     
           theme(
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)
          )
        )
}