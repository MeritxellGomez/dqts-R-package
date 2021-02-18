
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

  if(class(data) == 'ts'){
    data <- tsbox::ts_df(data)
    var_time_name <- 'time'
  }

  if(is.null(columnDate) & is.null(var_time_name)){warning("date or time variable should be given")}
  else if(is.null(columnDate)){columnDate <- which(colnames(data) == var_time_name)}
  else if(is.null(var_time_name)){var_time_name <- colnames(data)[columnDate]}

  if(metric == "Completeness"){HLCompleteness(data, method)}
  else if(metric == "TimeUniqueness"){HLTimeUniqueness(data, var_time_name, method)}
  else if(metric == 'Range'){HLRange(data, ranges, method)}
  else if(metric == 'Consistency' | metric == 'Typicality' | metric == 'Moderation'){HLNormality(data, metric)}
  else if(metric == "Timeliness"){HLTimeliness(data, columnDate, maxdif, units)}
  #else if(metric == "Conformity"){HLConformity(data)}
  else(stop('Incorrect metric name'))

}


# Handling Low Completeness -----------------------------------------------
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

HLTimeUniqueness <- function(data, var_time_name, method){

  if(is.null(var_time_name)){stop('Incorrect time variable name. The name of the time variable have to be written as an argument')}
  if(isFALSE(var_time_name %in% colnames(data))){stop('Incorrect time variable name. The name entered does not match any variable in the data set')}

  dupl <- duplicated(data[[var_time_name]])

  if(method == 'mean'){

    datesdupl <- unique(data[[var_time_name]][dupl])
    listdupl <- lapply(datesdupl, function(x) which(data[[var_time_name]] == x))
    imp <- lapply(listdupl, function(y) data[y,] %>% select(-var_time_name) %>% apply(., 2, function(x) mean(x, na.rm = TRUE)))

    #acabar esto. Ahora hay que meter los valores de imp en las posiciones de listdupl pero solo en las vars numericas


  }else if(method == 'deletion'){

    data <- data[!dupl,]

  }else(stop('Incorrect name of method to handle low Time Uniqueness'))

  return(data)

}



# Handling Low Range ------------------------------------------------------

HLRange <- function(data, ranges, method = 'mean'){

  #añadir que se pueda escoger method = mean (la media de los limits),
  #limits (si sale por arriba poner el max y si sale por abajo poner el min), imputemethods

  if(is.null(ranges)){ranges <- generateRangeData(data)}

  listout <- isoutofrange(data, ranges) #devuelve lista de variables y cada elemento contiene vector de indices out of range

  ind <- c(1:length(listout))[sapply(listout, function(x) !is.null(x))]

  if(method == 'mean'){
    data <- imputeRangesMean(data, ranges, ind, listout)
  }else if(method == 'maxmin'){
    data <- imputeRangesMaxMin(data, ranges, ind, listout)
  }else if(method == 'KNPTS'){
    #podemos ponerlos a NA y luego hacer imputacion
    data <- imputeRangesKNPTS(data, ranges, ind, listout)
  }else if(method == 'NA'){
    data <- imputeRangesNA(data, ranges, ind, listout)
  }else{
    data <- 'Method not correct'
  }

  return(data)
}


imputeRangesMean <- function(data, ranges, ind, listout){

  for (i in ind){
    data[[names(listout)[i]]][listout[[i]]] <- mean(ranges[[names(listout)[i]]])
  }
  return(data)
}

imputeRangesMaxMin <- function(data, ranges, ind, listout){

  for (i in ind){

    data[[names(listout)[i]]][listout[[i]]] <- ifelse(data[[names(listout)[i]]][listout[[i]]] < ranges[[names(listout)[i]]][1], ranges[[names(listout)[i]]][1],
                                                      ifelse(data[[names(listout)[i]]][listout[[i]]] > ranges[[names(listout)[i]]][2], ranges[[names(listout)[i]]][2], 'error'))

  }

  return(data)

}

imputeRangesNA <- function(data, ranges, ind, listout){

  for(i in ind){

    data[[names(listout)[i]]][listout[[i]]] <- NA

  }

  return(data)

}

imputeRangesKNPTS <- function(data, ranges, ind, listout){

  #darle una vuelta a ver si puedo aprovechar el de impute. Fijo que si

}


# Handling Low Normality  ------------------------------------------------

HLNormality <- function(data, metric){

  z <- ifelse(metric == 'Consistency', qnorm(0.9),
              ifelse(metric == 'Typicality', qnorm(0.975),
                     ifelse(metric == 'Moderation', qnorm(0.995), stop('Incorrect name of metric'))))


  listout <- isoutofnormality(data, metric)

  ind <- c(1:length(listout))[sapply(listout, function(x) !is.null(x))]

  a <- 4
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





