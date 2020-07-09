convertToDataFrame <- function(ts){

  if(class(ts) != 'ts'){stop('Data must be a ts object')}

  df <- as.data.frame(ts)

  f <- frequency(ts)
  start <- c(start(ts),1)
  end <- c(end(ts), 1)

  from <- lubridate::ymd(paste(start[1], "-", start[2], "-", start[3]))
  to <- lubridate::ymd(paste(end[1], "-", end[2], "-", end[3]))

  if(f==12){

    date <- seq(from, to, by="month")

  }

  df[['date']] <- date

  return(df)

}
