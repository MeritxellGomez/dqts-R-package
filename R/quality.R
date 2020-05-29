#####QUALITY FUNCTION#####

DQ <- function(data, columnDate, maxdif, dataref = NULL, ranges = NULL, weights = NULL, windows=FALSE, cte=TRUE, fixed=TRUE, nint=3, by=10){

  if(is.null(columnDate)){

    #buscar la forma de que detecte el fomato de fecha y le asigne la columna a columnDate

  }

  if(is.null(maxdif)){ #if maxdif is null, then the most frequent value is assigned

    diffs <- diff(data[,columnDate])

    uniq <- unique(diffs)

    maxdif <- uniq[which.max(tabulate(match(diffs, uniq)))]
  }


  if(isFALSE(windows)){
    myquality <- quality(data, columnDate, maxdif, dataref, ranges, weights)
  }else{

  source(here::here('R', 'movingwindows.R'))
  library(dplyr)

    minidf <- data %>% nrow() %>% wdwind(., cte, fixed, nint, by) %>% segwdw(data,.)

    myquality <- do.call(rbind,lapply(minidf, function(x) quality(x, columnDate = 1, maxdif = 30, dataref=NULL, ranges=NULL, weights=NULL)))

  }


  return(myquality)
}
