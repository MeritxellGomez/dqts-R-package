
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
handleDQ <- function(data, metric, columnDate = NULL, var_time_name=NULL, ranges = NULL, method = "mean", maxdif = NULL, units = NULL){

  if(class(data) == 'ts'){data <- tsbox::ts_df(data)}

  if(is.null(columnDate) & is.null(var_time_name)){warning("date or time variable should be given")}
  else if(is.null(columnDate)){columnDate <- which(colnames(data) == var_time_name)}
  else if(is.null(var_time_name)){var_time_name <- colnames(data)[columnDate]}

  if(metric == "Completeness"){HLCompleteness(data, method)}
  else if(metric == "TimeUniqueness"){HLTimeUniqueness(data, columnDate, var_time_name)}
  else if(metric == 'Range'){HLRange(data, ranges)}
  else if(metric == 'Consistency'){HLConsistency(data)}
  else if(metric == 'Typicality'){HLTypicality(data)}
  else if(metric == 'Moderation'){HLModeration(data)}
  else if(metric == "Timeliness"){HLTimeliness(data, columnDate, maxdif, units)}
  else if(metric == "Conformity"){HLConformity(data)}
  else(stop('Incorrect metric name'))

}


# Handling Low Completeness -----------------------------------------------

idlist <- function(idvec){

  idbreak <- which(diff(idvec) > 1)
  idbreak <- c(idbreak, length(idvec))
  n <- length(idbreak)

  idlist <- list()

  from <- 1

  for(i in 1:n){

    idlist[[i]] <- idvec[from:idbreak[i]]
    from <- idbreak[i] + 1

  }

  return(idlist)

}

imputation <- function(var,method, idna){

  if(method == "mean"){
    estim <- impmean(var = var, idna = idna)
  }else if(method == "KNPTS"){
    estim <- impKNPTS(var = var, idna = idna, future = TRUE)
  }else{
    estim <- 0
  }

  return(estim)

}


imputena <- function(method, var){

  idna <- which(is.na(var))
  idnalist <- idlist(idna)

  estim <- lapply(idnalist, function(x)imputation(var, method, x))

  #estim va a ser una lista donde cada elemento es la estimacion de esos indices de NA



  #repetir el proceso para cada intervalo de NA de la serie
  #devolver una lista con los vectores de predicciones de length el numero de NA seguidos
  return(estim)

}

HLCompleteness <- function(data, method="mean"){

  nacol <- apply(data, 2, function(x) sum(is.na(x)))

  #hay que añadir la condicion de que solo mire para las columnas numericas.
  #Alternativa: buscar forma de imputar fechas, factors, characters, ...

  imputation <- apply(data[which(nacol != 0)], 2, function(x) imputena(method, x))

  nvar <- length(which(nacol != 0))

  for(i in 1:nvar){
    nsub <- length(imputation[[i]])
    ind <- which(is.na(data[[names(imputation[i])]]))
    indexs <- idlist(ind)

    for(j in 1:nsub){
    data[[names(imputation[i])]][indexs[[j]]] <- imputation[[i]][[j]]
    }
  }

  return(data)

}



# Handling Time Uniqueness ------------------------------------------------

HLTimeUniqueness <- function(data, columnDate, var_time_name){

  if(is.null(var_time_name)){stop('Incorrect time variable name. The name of the time variable have to be written as an argument')}
  if(isFALSE(var_time_name %in% colnames(data))){stop('Incorrect time variable name. The name entered does not match any variable in the data set')}

  dupl <- duplicated(data[[var_time_name]])

  data <- data[!dupl,]

  return(data)

}



# Handling Low Range ------------------------------------------------------

HLRange <- function(data, ranges){

  if(is.null(ranges)){ranges <- generateRangeData(data)}

  listout <- isoutofrange(data, ranges)

  ind <- c(1:length(listout))[sapply(listout, function(x) !is.null(x))]

  for (i in ind){

    data[[names(listout)[i]]][listout[[i]]] <- mean(ranges[[names(listout)[i]]])

  }

  return(data)
}


#habria que crear funciones comunes para las metricas de Range, Consistency, Typicality y Moderation



# Handling Low Consistency ------------------------------------------------

HLConsistency <- function(data){

  #lo mismo que con Range.

}


# Handling Low Typicality -------------------------------------------------

HLTypicality <- function(data){

  #lo mismo que con Range.

}

# Handling Low Moderation -------------------------------------------------

HLModeration <- function(data){

  #lo mismo que con Range.

}



# Handling Low Timeliness -------------------------------------------------

HLTimeliness <- function(data, columnDate, maxdif, units){

  var_time_name <- colnames(data)[columnDate]

  first_date <- data[[var_time_name]][1]
  last_date <- data[[var_time_name]][nrow(data)]

  # if(units == "mins"){
  #     step <- 60*maxdif
  # }else if(units == "days"){
  #   step <- 3600*maxdif
  # }else if(units == "secs"){
  #   step <- maxdif
  # }else{
  #   stop('units should be one of mins, days, secs')
  # }

  dif<-as.numeric(diff(data[,columnDate]))

  outdif<-which(dif>maxdif)
  outdif_post <- outdif + 1

  l <- list()

  for (i in 1:length(outdif)){

    l[[i]] <- c(data[[var_time_name]][outdif[i]], data[[var_time_name]][outdif_post[i]])

  }

  df_list <- lapply(l, function(x)aux_timeliness(x, units = units, data = data, columnDate=columnDate))

  for (j in 1:length(df_list)){

    data <- rbind(data, df_list[[j]])

  }

  data <- data[order(data[[var_time_name]]),]

  rownames(data) <- c(1:nrow(data))

  return(data)
}

#vec contains two dates. output is a df with dates between and NA in the rest of vars
aux_timeliness <- function(vec, units, data, columnDate){

  n <- ncol(data) - 1

  missingtime <- seq(from = vec[1], to = vec[2], by = units)
  missingtime <- missingtime[-c(1,length(missingtime))]

  m <- length(missingtime)

  l<-list()

  for (i in 1:ncol(data)){

    if(i != columnDate){

      l[[i]] <- rep(NA, m)

    }else{

      l[[i]] <- missingtime

    }

  }

  aux <- data.frame(l)

  colnames(aux) <- colnames(data)

  return(aux)

}

# Handling Low Conformity -------------------------------------------------

HLConformity <- function(data){

  #varias opciones segun el problema que se tenga...

  #intentar cambiar las cosas de formato con un as.formato

  #coger las columnas de dataref y dejar solo esas en data.



}





