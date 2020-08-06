#' Deep analysis of data quality
#'
#' The data set is entered and the name of the metric to be analyzed in depth is indicated. Data quality can be evaluated by variables or the values in which an error occurs can be shown.
#'
#' @param data The data frame to be analyzed
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
deepDQ <- function(data, metric, columnDate=NULL, var_time_name = NULL, position = FALSE, dataref=NULL, ranges = NULL, maxdif, units="mins", missing=TRUE){

  if(class(data) == 'ts'){
    data <- tsbox::ts_df(data)
    columnDate <- 1
  }

  if(is.null(var_time_name)){var_time_name <- colnames(data)[columnDate]}

  if(metric == "Completeness"){deepdf <- deepCompleteness(data, var_time_name, position)}
  if(metric == "Timeliness"){deepdf <- deepTimeliness(data, columnDate, var_time_name, maxdif, units, missing)}
  if(metric == "TimeUniqueness"){deepdf <- deepTimeUniqueness(data, var_time_name)}
  if(metric == "Conformity"){deepdf <- deepConformity(data, dataref)}
  if(metric == "Range"){deepdf <- deepRange(data, ranges, var_time_name, position)}
  #añadir todas las otras metricas

  return(deepdf)

}

# Timeliness --------------------------------------------------------------


#study of timeliness. data is the set of values. columndate is an integer to indicate the position of the date variable
#maxdif is an integer to indicate the maximum difference allowed between two dates
#missing is a boolean: TRUE if we want to see all results and FALSE if we always want to see the missing intervals
deepTimeliness <- function(data, columnDate=NULL, var_time_name=NULL, maxdif, units="mins", missing=TRUE){

  #crear vector auxiliar de fechas que coja las fechas de la posicion 2 a la ultima
  #difftime(df$day[2], df$day[1], units = "mins") aplicar esto leyendo units del input y sin coger el ultimo valor de data

  if(!is.null(columnDate)){
    var_time_name <- colnames(data)[columnDate]
  }

  date_vec <- data[[var_time_name]]
  dif <- diff(date_vec)

  pos <- which(dif>maxdif)

  loss.start <- data[[var_time_name]][pos]
  loss.finish <- data[[var_time_name]][pos+1]

  waiting.time <- difftime(loss.finish, loss.start, units = units)

  missing.amount <- trunc(((as.numeric(waiting.time))/maxdif)-1)

  df.timeliness <- data.frame(loss.start, loss.finish, waiting.time, missing.amount)

  if(missing==FALSE){df.timeliness <- df.timeliness[-which(df.timeliness$missing.amount==0),]}

  return(df.timeliness)

}


#el input es la salida de un deepTimeliness y el numero de perdidas que se quieren mostrar
#ordena las perdidas temporales de mayor a menor
bigMissingTimes <- function(dT, n=1){

  dT %>% dplyr::arrange(desc(waiting.time)) %>% head(n)

}



# aggregate ---------------------------------------------------------------

#hay que hacer una función que agrupe los datos teniendo en cuenta la fecha. Cada 30 minutos que haga una media de la temperatura

agg <- function(data, var_time_name, m){

  data <- data %>% mutate('dateAggregated' = data[[var_time_name]])

  first <- data[[var_time_name]][1]
  after <- first + m

  last <- data[[var_time_name]][nrow(data)]

  dm <- as.numeric(difftime(last, first, units = "mins"))
  i_fin <- trunc(dm/30)

  for(i in 1:i_fin){

    ind <- which(first <= data[[var_time_name]] & data[[var_time_name]] < after)

    data[['dateAggregated']][ind] <- first #no consigo que copie bien la fecha... le cambia el formato

  firstaux <- first

  first <- after
  after <- after + m

  }

  data[['dateAggregated']][(max(ind) + 1) : nrow(data)] <- firstaux + m

  ind28 <- which(lubridate::minute(data$dateAggregated) == 28)
  lubridate::minute(data$dateAggregated[ind28]) <- 05
  lubridate::minute(data$dateAggregated[-ind28]) <- 35

  data <- data[,3:2]

  data <- aggregate(data,
                    by = list(data$dateAggregated),
                    FUN = mean)

  data <- data[, 2:3]

  return(data)

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


# Time Uniqueness ----------------------------------------------------------

deepTimeUniqueness <- function(data, var_time_name){

  columnDate <- which(colnames(data) == var_time_name)

  df <- as.data.frame(table(data[[columnDate]])) %>% arrange(desc(Freq)) %>% filter(Freq > 1)

  colnames(df) <- c(var_time_name, "Frequency")

  return(df)

}


# Conformity --------------------------------------------------------------

deepConformity <- function(data, dataref){

  if(is.null(dataref))stop('a reference data frame must be entered to compare')

  compare <- list()
  compare[['original data']] <- apply(data, 2, class)
  compare[['reference data']] <- apply(dataref, 2, class)

  return(compare)

}



# Range -------------------------------------------------------------------

deepRange <- function(data, ranges, var_time_name, position){

  if(is.null(ranges)){
    warning('Range data has been generated by default')
    ranges <- generateRangeData(data)
  }

  out <- isoutofrange(data, ranges)

  if(position){

    df <- lapply(out, function(x) data[[var_time_name]][x])

  }else{

    df <- lapply(out, function(x) length(x)/nrow(data))

  }

  return(df)
}


