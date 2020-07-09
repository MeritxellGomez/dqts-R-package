convertToDataFrame <- function(ts){
  #HAY UNA FUNCION EN EL PAQUETE TSBOX QUE HACE ESTOOOOO!!!!
  if(class(ts) != 'ts'){stop('Data must be a ts object')}

  df <- as.data.frame(ts)

  f <- frequency(ts)
  start <- start(ts)
  end <- end(ts)

  if(f==1){

    date<-seq(from, to, by="year")

  }else if(f==12){

    start <- c(start(ts),1)
    end <- c(end(ts), 1)
    from <- lubridate::ymd(paste(start[1], "-", start[2], "-", start[3]))
    to <- lubridate::ymd(paste(end[1], "-", end[2], "-", end[3]))
    date <- seq(from, to, by="month")

  }else if(f==4){

    quarters <- c('01','04','07','10')

  }else{
    date<-NA
  }

  df[['date']] <- date

  return(df)

}
