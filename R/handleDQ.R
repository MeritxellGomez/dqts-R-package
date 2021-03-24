#' Handle low data quality
#'
#' This function allows to correct the low data quality of one of the metrics. Once the process is done, it returns the data frame with some values modified. Do, the new value for the metric chosen is 1.
#'
#' @param data The data frame with low data quality to be corrected
#' @param metric the name of the metric to be corrected
#' @param var_time_name optional name of the time variable
#' @param ranges A data frame containing the minimum and maximum value allowed to each variable
#'
#' @return the data frame with the low quality values corrected
#' @export
#'
#' @examples
handleDQ <- function(data, metric, columnDate = NULL, var_time_name = NULL, ranges = NULL, dataref = NULL, method = "mean", maxdif = NULL, units = NULL){

  if(class(data) == 'ts'){
    data <- tsbox::ts_df(data)
    var_time_name <- 'time'
  }

  if(is.null(columnDate) & is.null(var_time_name)){warning("date or time variable should be given")}
  else if(is.null(columnDate)){columnDate <- which(colnames(data) == var_time_name)}
  else if(is.null(var_time_name)){var_time_name <- colnames(data)[columnDate]}

  if(metric == "Formats" | metric == "Names"){HLConformity(data, metric, method, dataref)}
  else if(metric == "Timeliness"){HLTimeliness(data, var_time_name, maxdif, units, method)}
  else if(metric == "TimeUniqueness"){HLTimeUniqueness(data, var_time_name, method)}
  else if(metric == 'Range'){HLRange(data, method, ranges)}
  else if(metric == 'Consistency' | metric == 'Typicality' | metric == 'Moderation'){HLNormality(data, metric)}
  else if(metric == "Completeness"){HLCompleteness(data, method)}
  else(stop('Incorrect metric name'))

}

# Handling Low Conformity -------------------------------------------------

HLConformity <- function(data, metric, dataref){

  #varias opciones segun el problema que se tenga...

  #intentar cambiar las cosas de formato con un as.formato

  #coger las columnas de dataref y dejar solo esas en data.



}

# Handling Time Uniqueness ------------------------------------------------

HLTimeUniqueness <- function(data, var_time_name, method){

  if(is.null(var_time_name)){stop('Incorrect time variable name. The name of the time variable have to be written as an argument')}
  if(isFALSE(var_time_name %in% colnames(data))){stop('Incorrect time variable name. The name entered does not match any variable in the data set')}

  dupl <- duplicated(data[[var_time_name]])

  if(method == 'mean' | method == 'median' | method == 'min' | method == 'max'){

    datesdupl <- unique(data[[var_time_name]][dupl])
    listdupl <- lapply(datesdupl, function(x) which(data[[var_time_name]] == x))
    imp <- lapply(listdupl, function(x) aux_timeuniqueness(x, data, var_time_name, method = method))
    df_imp <- do.call(rbind, imp)

    data <- data[-unlist(listdupl),]
    data <- rbind(data, df_imp)
    data <- data[order(data[[var_time_name]]),]
    rownames(data) <- c(1:nrow(data))

  }else if(method == 'deletion'){

    data <- data[!dupl,]

  }else(stop('Incorrect name of method to handle low Time Uniqueness'))

  return(data)

}

# Handling Low Timeliness -------------------------------------------------

HLTimeliness <- function(data, var_time_name, maxdif, units, method){

  date_vec <- data[[var_time_name]]
  n <- length(date_vec)
  dif <- difftime(date_vec[2:n], date_vec[1:(n-1)], units = units)

  outdif<-which(dif >= 2*maxdif)
  outdif_post <- outdif + 1

  l <- list()

  for (i in 1:length(outdif)){

    l[[i]] <- c(data[[var_time_name]][outdif[i]], data[[var_time_name]][outdif_post[i]])

  }

  df_list <- lapply(l, function(x)aux_timeliness(x, units = units, var_time_name = var_time_name))
  df_list <- lapply(df_list, function(x) aux_df_timeliness(x, var_time_name = var_time_name))
  missing_df <- do.call(rbind, df_list)

  if(method == 'missing'){
    data <- merge(data, missing_df, by = var_time_name, all = TRUE)
    rownames(data) <- c(1:nrow(data))
  }else if(method == 'mean' | method == 'median' | method == 'min' | method == 'max'){
    data <- aux_methods_timeliness(data, var_time_name, missing_df, method)
  }else{
    data <- 'Method not correct'
  }

  return(data)
}

# Handling Low Range ------------------------------------------------------

HLRange <- function(data, method, ranges){

  if(is.null(ranges)){
    warning('Range data frame should be given. The maximum and minimum values from a sample of original data have been taken as range data')
    ranges <- generateRangeData(data)
  }

  selcolout <- isoutofrange(data, ranges)

  out_positions <- outofrange(data, ranges)

  list_out <- lapply(out_positions, idlist)

  data <- impute(data, list_out, method, ranges = ranges)

  return(data)

}

# Handling Low Normality  ------------------------------------------------

HLNormality <- function(data, metric){

  rownames(data) <- c(1:nrow(data))

  out <- outofnormality(data)

  out_metric <- out[[metric]]

  perc_by_vars <- Normality(data, out, metric, group = FALSE) %>% lapply(., function(x) 1-x)

  for(i in 1:length(names(out_metric))){

    #pos_imputation <- sample(out_metric[[i]], size = nrow(data)*perc_by_vars[[names(out_metric)[i]]])

    data[[names(out_metric)[i]]][out_metric[[i]]] <- mean(data[[names(out_metric)[i]]])

  }

  return(data)

}

# Handling Low Completeness -----------------------------------------------
HLCompleteness <- function(data, method){

  selcolna <- apply(data, 2, function(x) ifelse(sum(is.na(x))!=0, TRUE, FALSE))

  na_positions <- lapply(data[which(selcolna != 0)],  function(x) which(is.na(x)))

  list_na <- lapply(na_positions, idlist)

  data <- impute(data = data, list_ids = list_na, method = method, ranges = NULL)

  return(data)

}
