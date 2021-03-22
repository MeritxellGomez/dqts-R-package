
# auxiliars TimeUniqueness ------------------------------------------------
aux_timeuniqueness <- function(vecdupl, data, var_time_name, method){

  data_no_time <- data %>% select(-var_time_name)
  date <- data[[var_time_name]][vecdupl[1]]

  if(method == 'mean'){
    m <- apply(data_no_time[vecdupl,], 2, function(x) mean(x, na.rm = TRUE))
  }else if(method == 'median'){
    m <- apply(data_no_time[vecdupl,], 2, function(x) median(x, na.rm = TRUE))
  }else if(method == 'min'){
    m <- apply(data_no_time[vecdupl,], 2, function(x) min(x, na.rm = TRUE))
  }else if(method == 'max'){
    m <- apply(data_no_time[vecdupl,], 2, function(x) max(x, na.rm = TRUE))
  }

  df_to_be_added <- data.frame(date, t(m))
  colnames(df_to_be_added) <- colnames(data)

  return(df_to_be_added)

}

# auxiliars Timeliness ----------------------------------------------------

#vec contains two dates. output is a df with dates between and NA in the rest of vars
aux_timeliness <- function(vec, units, var_time_name){

  missingtimes <- data.frame(seq(from = vec[1], to = vec[2], by = units))

  colnames(missingtimes) <- var_time_name

  return(missingtimes)

}

aux_df_timeliness <- function(x, var_time_name){

  n <- nrow(x)

  df <- data.frame(x[-c(1,n),])

  colnames(df) <- var_time_name

  return(df)
}

# auxiliars Range ---------------------------------------------------------




imputeRangesMeanRanges <- function(data, ranges, ind, listout){

  for (i in ind){
    data[[names(listout)[i]]][listout[[i]]] <- mean(ranges[[names(listout)[i]]])
  }
  return(data)
}


imputeRangesMean <- function(data, ranges, ind, listout){

  for (i in ind){
    data[[names(listout)[i]]][listout[[i]]] <- mean(data[[names(listout)[i]]][-listout[[i]]], na.rm = TRUE)

  }

  return(data)

}

imputeRangesMedian <- function(data, ranges, ind, listout){

  for (i in ind){
    data[[names(listout)[i]]][listout[[i]]] <- median(data[[names(listout)[i]]][-listout[[i]]], na.rm = TRUE)

  }

  return(data)

}

imputeRangesMaxMin <- function(data, ranges, ind, listout){

  for (i in ind){

    data[[names(listout)[i]]][listout[[i]]] <- ifelse(data[[names(listout)[i]]][listout[[i]]] < ranges[[names(listout)[i]]][1], ranges[[names(listout)[i]]][1],
                                                      ifelse(data[[names(listout)[i]]][listout[[i]]] > ranges[[names(listout)[i]]][2], ranges[[names(listout)[i]]][2], 'error'))

  }

  return(data)

}

imputeRangesNA <- function(data, ranges, ind, listout){

  for(i in ind){

    data[[names(listout)[i]]][listout[[i]]] <- NA

  }

  return(data)

}

imputeRangesKNPTS <- function(data, ranges, ind, listout){

  #darle una vuelta a ver si puedo aprovechar el de impute. Fijo que si

}







# auxiliars Completeness --------------------------------------------------
#imputation

d_euclidean <- function(vec_x, vec_y){

  d <- sqrt(sum((vec_x - vec_y)^2))

  return(d)

}


#funcion que de un data frame salido de la funcion distance_knn (ordenado), escoger los k más cercanos sin NAS
kneighbors <- function(distk, k){

  distk <- distk[complete.cases(distk),]

  kneig <- distk[1:k,]

  return(kneig)

}


#funcion que calcula la distancia entre los datos que se introducen (VECTOR) y
#todos los intervalos anteriores. Devuelve df con 3 cols: initialindex, finalindex, distancevalue
#future = TRUE permite usar datos futuros
distance_knn <- function(datavar, idna = idna, h, npred, dist = "Euclidean", future = TRUE){

  firstna <- min(idna)

  #horizon data to be compared
  h_data <- datavar[(firstna - h) : (firstna - 1)]
  nna <- length(which(is.na(h_data)))

  while(nna != 0){ #controlar que en el horizon no haya NA

    h_data <- datavar[(firstna - h - nna) : (firstna - 1)]

    nna2 <- length(which(is.na(h_data)))

    if(nna2 == nna){
      h_data <- na.omit(h_data)
      nna <- length(which(is.na(h_data)))
    }else{
      nna <- nna2
    }

  }

  #built dataframe to be written
  dfdist <- data.frame(initial.value = integer(), final.value = integer(), distances = double(), stringsAsFactors = FALSE)

  for(i in 1:npred){

    dfdist[,i+3] <- double()
    colnames(dfdist)[i+3] <- paste0('futurevalue_',i)
  }


  #look for the distances with previous data
  initial1_f <- firstna - h - npred

  if(initial1_f > 0){
    for (i in 1:initial1_f){

      int <- datavar[i:(i+h-1)]

      d_aux <- d_euclidean(int, h_data)

      dfdist[i,] <- NA
      dfdist[['initial.value']][i] <- i
      dfdist[['final.value']][i] <- i+h-1
      dfdist[['distances']][i] <- d_aux

      dfdist[i, 4:(3+npred)] <- datavar[(i+h) : (i+h-1+npred)]

    }
  }

  #if future == TRUE, look for the distances with future data
  if(future){

    initial2_0 <- max(idna) + 1
    initial2_f <- length(datavar) - h - npred + 1
    j <- 1

    for(i in initial2_0:initial2_f){

      int <- datavar[i:(i+h-1)]

      d_aux <- d_euclidean(int, h_data)

      dfdist[initial1_f+j,] <- NA
      dfdist[['initial.value']][initial1_f+j] <- i
      dfdist[['final.value']][initial1_f+j] <- i+h-1
      dfdist[['distances']][initial1_f+j] <- d_aux

      dfdist[initial1_f+j, 4:(3+npred)] <- datavar[(i+h) : (i+h-1+npred)]

      j<-j+1

    }

  }


  library(dplyr)
  dfdist <- dfdist %>% arrange(distances)

  return(dfdist)

}


predictknn<-function(datavar, kneighbors, npred, pond){

  m <- as.matrix(kneighbors[4:ncol(kneighbors)])

  #weighted average
  if(pond){
    w <- (1/(kneighbors[['distances']])^2)
    nw <- w/(sum(w))
    pred <- (nw %*% m)
  }else{
    pred <- apply(m, 2, function(x)mean(x))
  }

  return(pred)

}




idlist <- function(idna){

  idbreak <- which(diff(idna) > 1)
  idbreak <- c(idbreak, length(idna))
  n <- length(idbreak)

  idlist <- list()

  from <- 1

  for(i in 1:n){

    idlist[[i]] <- idna[from:idbreak[i]]
    from <- idbreak[i] + 1

  }

  return(idlist)

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

impKNFTS <- function(var, idna, future = TRUE){

  firstna <- min(idna)

  trainset <- data[-idna,]
  trainset <- createTimeFeatures(trainset, var_time_name)

  #caret::train() ... no tengo idea de como escribir esto cogiendo los nombres de las variables que toca

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

  return(estim)

}
