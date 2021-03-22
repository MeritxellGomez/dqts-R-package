
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



