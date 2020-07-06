#' Data quality from a data set
#'
#' Takes a data set and the conditions to compute the data quality and returns the values of all metrics
#' @param data A data frame to be analyzed
#' @return A data frame containing the values of all quality metrics in columns
#' @importFrom magrittr %>%
#' @export
DQ <- function(data, columnDate = NULL, maxdif = NULL, dataref = NULL, ranges = NULL, weights = NULL, windows=FALSE, cte=TRUE, fixed=TRUE, nint=3, by=10){

  if(is.null(columnDate)){
    n <- length(which(sapply(data, class)=='Date'))
    if(n==1){
      columnDate <- which(sapply(data, class)=='Date')
    }else if(n==0){
      stop('No variable with Date format')
    }else{
      stop('More than one variable with Date format')
    }
  }

  if(is.null(dataref)){
      warning('Reference data frame should be given. The original data has been taken as reference data')
      dataref <- data
  }

  if(is.null(ranges)){
    warning('Range data frame should be given. The maximum and minimum values from the original data have been taken as range data')
    ranges <- generateRangeData(data)
  }

  if(is.null(maxdif)){ #if maxdif is null, then the most frequent value is assigned
    diffs <- diff(data[,columnDate])
    uniq <- unique(diffs)
    maxdif <- uniq[which.max(tabulate(match(diffs, uniq)))]
  }

  if(isFALSE(windows)){
    myquality <- quality(data, columnDate, maxdif, dataref, ranges, weights)
  }else{
    minidf <- data %>% nrow() %>% wdwind(., cte, fixed, nint, by) %>% segwdw(data,.)
    myquality <- do.call(rbind,lapply(minidf, function(x) quality(x, columnDate, maxdif, dataref, ranges, weights)))
  }

  return(myquality)
}
