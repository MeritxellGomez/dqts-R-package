###crear una función que lea los datos y la métrica a corregir y aplique en cada caso la función que deba

handle_DQ <- function(data, metric, var_time_name=NULL){

  if(metric == "Completeness"){HLCompleteness(data)}
  else if(metric == "TimeUniqueness"){HLTimeUniqueness(data, var_time_name)}
  else if(metric == 'Range'){HLRange(data)}
  else if(metric == 'Consistency'){HLConsistency(data)}
  else if(metric == 'Typicality'){HLTypicality(data)}
  else if(metric == 'Moderation'){HLModeration(data)}
  else if(metric == "Timeliness"){HLTimeliness(data)}
  else if(metric == "Conformity"){HLConformity(data)}
  else(stop('Incorrect metric name'))

}


# Handling Low Completeness -----------------------------------------------

HLCompleteness <- function(data){



  #identificar la cantidad de NA's

  #imputar los NA's

  #devolver el data con los NA imputados

}



# Handling Time Uniqueness ------------------------------------------------

HLTimeUniqueness <- function(data, var_time_name){

  if(is.null(var_time_name)){stop('Incorrect time variable name. The name of the time variable have to be written as an argument')}
  if(isFALSE(var_time_name %in% colnames(data))){stop('Incorrect time variable name. The name entered does not match any variable in the data set')}

  dupl <- duplicated(data[[var_time_name]])

  data <- data[!dupl,]

  return(data)

}



# Handling Low Range ------------------------------------------------------

HLRange <- function(data){

  #coger los elementos que estan fuera de rango

  #opciones : eliminarlos, imputarlos por la media, imputarlos por max/min permitido segun si sobrepasan o no

  #devolver los datos limpios

}


#habria que crear funciones comunes para las metricas de Range, Consistency, Typicality y Moderation



# Handling Low Consistency ------------------------------------------------

HLConsistency <- function(data){

  #lo mismo que con Range.

}


# Handling Low Timeliness -------------------------------------------------

HLTimeliness <- function(data, var_time_name, maxdif, units){

  first_date <- data[[var_time_name]][1]
  last_date <- data[[var_time_name]][nrow(data)]

  if(units == "mins"){
      step <- 60*maxdif
  }else if(units == "days"){
    step <- 3600*maxdif
  }else if(units == "secs"){
    step <- maxdif
  }else{
    stop("incorrect units")
  }

  #mirar cuando timeliness es mayor que maxdif

  #en esos huecos meter tantas fechas como se hayan perdido y asignarle al resto de valores NA

}



# Handling Low Conformity -------------------------------------------------

HLConformity <- function(data){

  #varias opciones segun el problema que se tenga...

  #intentar cambiar las cosas de formato con un as.formato

  #coger las columnas de dataref y dejar solo esas en data.



}





