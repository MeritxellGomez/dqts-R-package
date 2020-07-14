
#in-depth quality analysis

deepDQ <- function(data, metric, var_time_name, position = FALSE){

  if(metric == "Completeness"){deepdf <- deepCompleteness(data, var_time_name, position)}
  if(metric == "Timeliness"){deepdf <- deepTimeliness(data, columnDate=NULL, var_time_name=NULL, maxdif, units="mins", missing=TRUE)}
  if(metric == "TimeUniqueness"){deepdf <- deepTimeUniqueness(data, var_time_name)}
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

  waiting.time <- loss.finish - loss.start

  missing.amount <- trunc(((as.numeric(loss.finish - loss.start))/maxdif)-1)

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




# TimeUniqueness ----------------------------------------------------------

deepTimeUniqueness <- function(data, var_time_name){

  columnDate <- which(colnames(data) == var_time_name)

  df <- as.data.frame(table(data[[columnDate]])) %>% arrange(desc(Freq)) %>% filter(Freq > 1)

  colnames(df) <- c(var_time_name, "Frequency")

  return(df)

}
