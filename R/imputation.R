
idlist <- function(idvec){

  idbreak <- which(diff(idvec) > 1)
  idbreak <- c(idbreak, length(idvec))
  n <- length(idbreak)

  idlist <- list()

  from <- 1

  for(i in 1:n){

    idlist[[i]] <- idvec[from:idbreak[i]]
    from <- idbreak[i] + 1

  }

  return(idlist)

}

imputation <- function(var,method, idna){

  if(method == "mean"){
    estim <- impmean(var = var, idna = idna)
  }else if(method == "KNPTS"){
    estim <- impKNPTS(var = var, idna = idna, future = TRUE)
  }else if(method == "mean2"){
    estim <- impmean2(var = var, idna = idna, future = TRUE)
  }else{
    estim <- 0
  }

  return(estim)

}


imputena <- function(method, var){

  idna <- which(is.na(var))
  idnalist <- idlist(idna)

  estim <- lapply(idnalist, function(x)imputation(var, method, x))

  #estim va a ser una lista donde cada elemento es la estimacion de esos indices de NA



  #repetir el proceso para cada intervalo de NA de la serie
  #devolver una lista con los vectores de predicciones de length el numero de NA seguidos
  return(estim)

}
impmean<- function(var, idna, future = TRUE){

  last <- min(idna) - 1
  trainset <- var[1:last]

  if(future){
    trainset <- c(trainset, var[(max(idna)+1) : length(var)])
  }

  pred <- rep(mean(trainset, na.rm = TRUE), length(idna))

  return(pred)

}

impmean2<- function(var, idna, future = TRUE){

  a <- var[min(idna) - 1]
  b <- var[max(idna) + 1]

  value <- mean(c(a,b))

  pred <- rep(value, length(idna))

  return(pred)

}


impKNPTS<-function(var, idna, future = TRUE){

  npred <- length(idna)

  #dejo el crossvalidation para mas tarde... de momento escojo un valor cualquiera de k
  #cvres <- cvknnvar(var.train = traincv, var.test = testcv, h = n, npred = n, kmax = 5, pond = FALSE)

  # k <- cvres['k'][which(cvres['DTW'] == min(cvres['DTW']))]
  #
  # dvar <- var[1:(min(idna) - 1)]
  # pred <- predict.knn(datavar = dvar, kneighbors = k, npred = n, pond = FALSE)

  distknn <- distance_knn(datavar = var, idna = idna, h = 3, npred = npred, dist = "Euclidean", future = TRUE)

  kn <- kneighbors(distknn, k = 3)

  pred <- predictknn(datavar = var, kneighbors = kn, npred = npred, pond = FALSE)

  return(pred)

}


impKNFTS <- function(var, idna, future = TRUE){

  firstna <- min(idna)

  trainset <- data[-idna,]
  trainset <- createTimeFeatures(trainset, var_time_name)

  #caret::train() ... no tengo idea de como escribir esto cogiendo los nombres de las variables que toca

}

createTimeFeatures <- function(trainset, var_time_name){

  var <- trainset[[var_time_name]]

  trainset[['year']] <- lubridate::year(var)
  trainset[['month']] <- lubridate::month(var)
    trainset[['sinmonth']] <- sin(2*pi*trainset[['month']] / 12)
    trainset[['cosmonth']] <- cos(2*pi*trainset[['month']] / 12)
  trainset[['day']] <- lubridate::day(var)
    trainset[['sinday']] <- sin(2*pi*trainset[['day']] / 31)
    trainset[['cosday']] <- cos(2*pi*trainset[['day']] / 31)
  trainset[['hour']] <- lubridate::hour(var)
    trainset[['sinhour']] <- sin(2*pi*trainset[['hour']] / 24)
    trainset[['coshour']] <- cos(2*pi*trainset[['hour']] / 24)
  trainset[['minute']] <- lubridate::minute(var)
    trainset[['sinminute']] <- sin(2*pi*trainset[['minute']] / 60)
    trainset[['cosminute']] <- cos(2*pi*trainset[['minute']] / 60)
  trainset[['second']] <- lubridate::second(var)
    trainset[['sinsecond']] <- sin(2*pi*trainset[['second']] / 60)
    trainset[['cossecond']] <- cos(2*pi*trainset[['second']] / 60)

  return(trainset)
}
