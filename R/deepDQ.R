#' Deep analysis of data quality
#'
#' The data set is entered and the name of the metric to be analyzed in depth is indicated. Data quality can be evaluated by variables or the values in which an error occurs can be shown.
#'
#' @param data The data frame or time series to be analyzed
#' @param metric The name of the metric to be analyzed
#' @param var_time_name The name of the variable containing the date or time information
#' @param position Logical argument. If TRUE date or time information is added to the results
#' @param dataref A data frame with the correct format to compare the current data
#' @param ranges A data frame containing the minimum and maximum value allowed to each variable
#'
#' @return Detailed information about the failures occurred in the chosen metric
#' @export
#'
#' @examples
deepDQ <- function(data, metric, columnDate=NULL, var_time_name = NULL, position = FALSE, dataref=NULL, ranges = NULL, maxdif = NULL, units="secs"){

  if(class(data) == 'ts'){
    data <- tsbox::ts_df(data)
    columnDate <- 1
  }

  if(is.null(columnDate) & is.null(var_time_name)){warning("date or time variable should be given")}
  else if(is.null(columnDate)){columnDate <- which(colnames(data) == var_time_name)}
  else if(is.null(var_time_name)){var_time_name <- colnames(data)[columnDate]}

  if(metric == "Completeness"){deepdf <- deepCompleteness(data, var_time_name, position)}
  if(metric == "Timeliness"){deepdf <- deepTimeliness(data, var_time_name, maxdif, units)}
  if(metric == "TimeUniqueness"){deepdf <- deepTimeUniqueness(data, var_time_name)}
  if(metric == "Formats" | metric == "Names"){deepdf <- deepConformity(data, dataref, metric)}
  if(metric == "Range"){deepdf <- deepRange(data, ranges, var_time_name, position)}
  if(metric == "Consistency" | metric == 'Typicality' | metric == 'Moderation'){deepdf <- deepNormality(data, metric = metric, var_time_name, position)}

  return(deepdf)

}

# Conformity --------------------------------------------------------------

deepConformity <- function(data, dataref, metric){

  if(is.null(dataref)){
    warning('Reference data frame should be given. A sample of original data has been taken as reference data')
    dataref <- generateReferenceData(data)
  }

  if(metric == 'Formats'){

    formats <- lapply(data, class)
    formats <- lapply(formats, function(x) if(length(x) != 1){x <- paste(x, collapse = " ")}else{x <- x})
    formats <- data.frame(formats, stringsAsFactors = FALSE)

    df <- data.frame(Reference = t(dataref), Data = t(formats), stringsAsFactors = FALSE)

  }

  if(metric == 'Names'){

    col <- colnames(data)
    colref <- colnames(dataref)

    df <- data.frame(Reference = colref, Data = col)

  }

  logicals <- !apply(df, 1, function(x) x[1] == x[2])

  deep_conf <- df[logicals,]

  return(deep_conf)

}




# Time Uniqueness ----------------------------------------------------------

deepTimeUniqueness <- function(data, var_time_name){

  library(dplyr)

  columnDate <- which(colnames(data) == var_time_name)

  df <- as.data.frame(table(data[[columnDate]])) %>% arrange(desc(Freq)) %>% filter(Freq > 1)

  colnames(df) <- c(var_time_name, "Frequency")

  return(df)

}


# Timeliness --------------------------------------------------------------

#study of timeliness. data is the set of values. columndate is an integer to indicate the position of the date variable
#maxdif is an integer to indicate the maximum difference allowed between two dates
#missing is a boolean: TRUE if we want to see all results and FALSE if we always want to see the missing intervals
deepTimeliness <- function(data, var_time_name, maxdif, units){

  if(units == 'months'){
    maxdif <- 31
    units2 <- 'days'
  }else{
    units2 <- units
  }

  date_vec <- data[[var_time_name]]
  n <- length(date_vec)
  dif <- difftime(date_vec[2:n], date_vec[1:(n-1)], units = units)

  pos <- which(dif>maxdif)

  loss.start <- data[[var_time_name]][pos]
  loss.finish <- data[[var_time_name]][pos+1]

  waiting.time <- difftime(loss.finish, loss.start, units = units2)

  if(units == 'months'){
    missing.amount <- round(((as.numeric(abs(waiting.time)))/maxdif)-1)
  }else if(units == 'days'){
    missing.amount <- trunc(((as.numeric(abs(waiting.time)))/maxdif))
  }else{
    missing.amount <- trunc(((as.numeric(abs(waiting.time)))/maxdif)-1)
  }

  df.timeliness <- data.frame(loss.start, loss.finish, waiting.time, missing.amount)

  df.timeliness.filt <- subset(df.timeliness, missing.amount != 0)

  return(df.timeliness.filt)

}



# Range -------------------------------------------------------------------

deepRange <- function(data, ranges, var_time_name, position){

  if(is.null(ranges)){
    warning('Range data frame should be given. The maximum and minimum values from a sample of original data have been taken as range data')
    ranges <- generateRangeData(data)
  }

  out <- outofrange(data, ranges)

  if(position){

    df <- lapply(out, function(x) data[[var_time_name]][x])
    dfn <- plyr::ldply(df, rbind)
    for(i in 2:ncol(dfn)){dfn[,i] <- lubridate::as_datetime(dfn[,i])}

  }else{

    df <- lapply(out, function(x) 1-(length(x)/nrow(data)))
    dfn <- plyr::ldply(df, rbind)
    colnames(dfn) <- c('Variables', 'Metric Value')
  }

  return(dfn)
}






# Normality ---------------------------------------------------------------

deepNormality <- function(data, metric, var_time_name, position){

  out <- outofnormality(data)
  out_metric <- out[[metric]]

  if(position){

    df <- lapply(out_metric, function(x) data[[var_time_name]][x])
    dfn <- plyr::ldply(df, rbind)
    for(i in 2:ncol(dfn)){dfn[,i] <- lubridate::as_datetime(dfn[,i])}

  }else{

    df <- lapply(out_metric, function(x) 1-(length(x)/nrow(data)))
    dfn <- plyr::ldply(df, rbind)
    colnames(dfn) <- c('Gaussian Variables', 'Metric Value')
  }

  return(dfn)

}

# Completeness ------------------------------------------------------------

deepCompleteness <- function(data, var_time_name, position){

  if(position){
    deepcomp <- apply(data, 2, function(x) data[[var_time_name]][which(is.na(x))])
  }else{
    deepcomp<-apply(data, 2, function(x){1-(sum(is.na(x))/nrow(data))})
  }
  return(deepcomp)
}

