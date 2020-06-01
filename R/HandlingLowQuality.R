###crear una función que lea los datos y la métrica a corregir y aplique en cada caso la función que deba

# HLQ <- function(data, metric){
#
#
# }


# Handling Low Timeliness -------------------------------------------------

HLTimeliness <- function(data, var_time_name, maxdif, units){

  first_date <- data[[var_time_name]][1]
  last_date <- data[[var_time_name]][nrow(data)]

  if(units == "mins"){
      step <- 60*maxdif
  }else if(units == "days"){
    step <- 3600*maxdif
  }else if(units == "secs"){
    step <- maxdif
  }else{
    stop("incorrect units")
  }

  #mirar cuando timeliness es mayor que maxdif

  #en esos huecos meter tantas fechas como se hayan perdido y asignarle al resto de valores NA

}
