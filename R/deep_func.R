
#in-depth quality analysis



# Timeliness --------------------------------------------------------------


#study of timeliness. data is the set of values. columndate is an integer to indicate the position of the date variable
#maxdif is an integer to indicate the maximum difference allowed between two dates
#missing is a boolean: TRUE if we want to see all results and FALSE if we always want to see the missing intervals
deepTimeliness <- function(data, columnDate, maxdif, missing=FALSE){

  dif <- diff(data[,columnDate])
  pos <- which(dif>maxdif)

  loss.start <- data[pos, columnDate]
  loss.finish <- data[pos+1, columnDate]

  waiting.time <- loss.finish - loss.start

  missing.amount <- trunc(((as.numeric(loss.finish - loss.start))/maxdif)-1)

  df.timeliness <- data.frame(loss.start, loss.finish, waiting.time, missing.amount)

  if(missing==FALSE){df.timeliness <- df.timeliness[-which(df.timeliness$missing.amount==0),]}

  return(df.timeliness)

}





# aggregate ---------------------------------------------------------------

#hay que hacer una función que agrupe los datos teniendo en cuenta la fecha. Cada 30 minutos que haga una media de la temperatura

agg <- function(data, var_time_name, m){
  browser()
  data <- data %>% mutate('aggregateDate' = 1)

  first <- data[[var_time_name]][1]
  print(class(first))
  last <- first + m


  for(i in 1:3){

    ind <- which(first <= data[[var_time_name]] & data[[var_time_name]] < last)

    data['aggregateDate'][ind] <- first #no consigo que copie bien la fecha... le cambia el formato


  first <- last
  last <- last + m

  }

  return(data)

}
