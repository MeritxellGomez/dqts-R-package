#####QUALITY FUNCTION#####

DQ <- function(data, columnDate, maxdif, dataref, ranges, weights, windows){

  if(is.null(columnDate)){

    #buscar la forma de que detecte el fomato de fecha y le asigne la columna a columnDate

  }

  if(is.null(maxdif)){ #if maxdif is null, then the most frequent value is assigned

    diffs <- diff(data[,columnDate])

    uniq <- unique(diffs)

    maxdif <- uniq[which.max(tabulate(match(diffs, uniq)))]
  }


  if(is.null(windows)){
    myquality <- quality(data, columnDate, maxdif, dataref, ranges, weights)
  }else{

    #incluir que pase la funcion de calidad por cada una de las ventanas que se elija

  }




  return(myquality)
}
