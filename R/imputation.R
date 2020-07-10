
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
