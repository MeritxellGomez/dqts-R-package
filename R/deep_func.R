
#in-depth quality analysis



# Timeliness --------------------------------------------------------------

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
