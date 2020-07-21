
impmean<- function(var, idna, future = TRUE){

  last <- min(idna) - 1
  trainset <- var[1:last]

  if(future){
    trainset <- c(trainset, var[(max(idna)+1) : length(var)])
  }

  pred <- rep(mean(trainset, na.rm = TRUE), length(idna))

  return(pred)

}


impKNPTS<-function(var, idna, future = TRUE){
  browser()
  npred <- length(idna)

  #dejo el crossvalidation para mas tarde... de momento escojo un valor cualquiera de k
  #cvres <- cvknnvar(var.train = traincv, var.test = testcv, h = n, npred = n, kmax = 5, pond = FALSE)

  # k <- cvres['k'][which(cvres['DTW'] == min(cvres['DTW']))]
  #
  # dvar <- var[1:(min(idna) - 1)]
  # pred <- predict.knn(datavar = dvar, kneighbors = k, npred = n, pond = FALSE)

  distknn <- distance_knn(datavar = var, idna = idna, h = 3, npred = npred, dist = "Euclidean", future = TRUE)

  kn <- kneighbors(distknn, k = 2)

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
