#####QUALITY FUNCTION#####

DQ <- function(data, columnDate, maxdif, dataref, ranges, weights, windows=FALSE, cte, fixed, nint){

  if(is.null(columnDate)){

    #buscar la forma de que detecte el fomato de fecha y le asigne la columna a columnDate

  }

  if(is.null(maxdif)){ #if maxdif is null, then the most frequent value is assigned

    diffs <- diff(data[,columnDate])

    uniq <- unique(diffs)

    maxdif <- uniq[which.max(tabulate(match(diffs, uniq)))]
  }




  if(isFALSE(windows)){
    myquality <- quality(data, columnDate, maxdif, dataref, ranges, weights)
  }else{

    ##AÑADIR AQUI LA FUNCION MOVINGWINDOW: Crear columna en el DF que indique el numero de ventana

    #myquality será un data frame en el que se aplica la funcion quality en cada ventana

  }


  return(myquality)
}
