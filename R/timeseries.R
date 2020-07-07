convertToDataFrame <- function(ts){

  if(class(ts) != 'ts'){stop('Data must be a ts object')}

  df <- as.data.frame(ts)
  df[['date']] <- as.Date(time(ts))

  return(df)

}
