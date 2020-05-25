
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
