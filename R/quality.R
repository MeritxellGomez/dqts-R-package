#' Data quality from a data set
#'
#' Takes a data set and the conditions to compute the data quality and returns the values of all metrics (Completeness, Completeness Observations, Completeness Variables, Time Uniqueness, Range, Consistency, Typicality, Moderation, Timeliness, Conformity) and the general value of data quality
#'
#' @param columnDate A number indicating in which column the date variable is
#' @param maxdif The maximum difference allowed between two consecutive dates
#' @param dataref A data frame with the correct format to compare the current data
#' @param ranges A data frame containing the minimum and maximum value allowed to each variable
#' @param weights A vector of length 10 indicating the weights to compute the weighted average
#' @param windows Logical argument. If TRUE the metrics will be calculated by windows
#' @param cte Logical argument. If TRUE the length of the windows will be the same
#' @param fixed Logical argument. If TRUE the initial value of each window is the same.
#' @param nint The number of windows
#' @param by The time difference between the initial value of each window
#' @param data The data frame to be analyzed
#'
#' @return A data frame containing the values of all quality metrics in columns
#' @importFrom magrittr %>%
#' @export
DQ <- function(data, columnDate = NULL, var_time_name = NULL, maxdif = NULL, units = 'secs', dataref = NULL, ranges = NULL, weights = NULL, windows=FALSE, initialWindow = 60, skip = 10, fixedWindow = TRUE){

  if(class(data) == 'ts'){data <- tsbox::ts_df(data)}

  if(is.null(columnDate) & is.null(var_time_name)){warning("date or time variable should be given")}
  else if(is.null(columnDate)){columnDate <- which(colnames(data) == var_time_name)}
  else if(is.null(var_time_name)){var_time_name <- colnames(data)[columnDate]}

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
      warning('Reference data frame should be given. A sample of original data has been taken as reference data')
      dataref <- generateReferenceData(data)
  }

  if(is.null(ranges)){
    warning('Range data frame should be given. The maximum and minimum values from a sample of original data have been taken as range data')
    ranges <- generateRangeData(data)
  }

  if(is.null(maxdif)){ #if maxdif is null, then the most frequent value is assigned
    warning('Maxdif should be given. An estimation of maxdif value is calculated from a sample of original data')
    maxdif <- generateMaxDif(data, columnDate)
  }

  if(is.null(weights)){
    weights<-c(rep((1/11),11))
  }

  if(!normalvars(data)){
    weights[c(6,7,8)] <- 0
    weights[c(1:5,9:11)] <- 1/8
  }

  if(isFALSE(windows)){
    myquality <- quality(data, columnDate, maxdif, units, dataref, ranges, weights)
  }else{
    minidf <- data %>% nrow() %>% windowpartitions(., initialWindow, skip, fixedWindow) %>% segwdw(data,.)
    myquality <- do.call(rbind,lapply(minidf, function(x) quality(x, columnDate, maxdif, units, dataref, ranges, weights)))
  }

  return(myquality)
}
