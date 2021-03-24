
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

aux_methods_timeliness <- function(data, var_time_name, missing_df, method){

  if(method == 'mean'){
    data_to_add_1 <- data %>% select(-var_time_name) %>% summarise_all(function(x) mean(x, na.rm = TRUE))
  }else if(method == 'median'){
    data_to_add_1 <- data %>% select(-var_time_name) %>% summarise_all(function(x) median(x, na.rm = TRUE))
  }else if(method == 'min'){
    data_to_add_1 <- data %>% select(-var_time_name) %>% summarise_all(function(x) min(x, na.rm = TRUE))
  }else if(method == 'max'){
    data_to_add_1 <- data %>% select(-var_time_name) %>% summarise_all(function(x) max(x, na.rm = TRUE))
  }else{
    data <- 'Method not correct'
  }

  data_to_add_n <- do.call('rbind', replicate(nrow(missing_df), data_to_add_1, simplify = FALSE))

  missing_df_mean <- cbind(missing_df, data_to_add_n)

  data <- rbind(data, missing_df_mean)
  data <- data[order(data[[var_time_name]]),]
  rownames(data) <- c(1:nrow(data))

  return(data)

}


# auxiliars Range and Completeness ---------------------------------------------------------

impute <- function(data, list_ids, method, ranges){

  nvar <- length(list_ids)

  l <- list()
  for(i in 1:nvar){

    name_var <- names(list_ids)[i]
    n <- length(list_ids[[i]])

    var_na <- list_ids[[i]]

    if(!is.null(ranges)){
      ranges_var <- ranges[[name_var]]
    }

    l[[i]] <- lapply(var_na, function(x) impute_vars(var = data[[name_var]], method = method, id = x, ranges = ranges_var))
    names(l)[i] <- name_var

    for(j in 1:n){

      data[[name_var]][list_ids[[name_var]][[j]]] <- l[[name_var]][[j]]

    }

  }

  return(data)

}

impute_vars <- function(var, method, id, ranges){

  if(method == "mean" | method == 'median' | method == 'min' | method == 'max'){
    estim <- impsimple(var = var, id = id, method = method, future = TRUE)
  }else if(method == "KNPTS"){
    estim <- impKNPTS(var = var, id = id, future = TRUE)
  }else if(method == "mean2"){
    estim <- impmean2(var = var, id = id, future = TRUE)
  }else if(method == 'meanranges'){
    estim <- impRangesMean(id = id, ranges)
  }else{
    estim <- NA
  }

  return(estim)

}

idlist <- function(id){

  idbreak <- which(diff(id) > 1)
  idbreak <- c(idbreak, length(id))
  n <- length(idbreak)

  idlist <- list()

  from <- 1

  for(i in 1:n){

    idlist[[i]] <- id[from:idbreak[i]]
    from <- idbreak[i] + 1

  }

  return(idlist)

}

#KNPTS

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
distance_knn <- function(datavar, id = id, h, npred, dist = "Euclidean", future = TRUE){

  firstna <- min(id)

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

    initial2_0 <- max(id) + 1
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
    pred <- apply(m, 2, function(x)mean(x, na.rm = TRUE))
  }

  return(pred)

}

impsimple<- function(var, id, method, future){

  last <- min(id) - 1
  trainset <- var[1:last]

  if(future){
    trainset <- c(trainset, var[(max(id)+1) : length(var)])
  }

  if(method == 'mean'){
    pred <- rep(mean(trainset, na.rm = TRUE), length(id))
  }else if(method == 'median'){
    pred <- rep(median(trainset, na.rm = TRUE), length(id))
  }else if(method == 'min'){
    pred <- rep(min(trainset, na.rm = TRUE), length(id))
  }else if(method == 'max'){
    pred <- rep(max(trainset, na.rm = TRUE), length(id))
  }else{
    pred <- 0
  }

  return(pred)

}

impmean2<- function(var, id, future = TRUE){

  a <- var[min(id) - 1]
  b <- var[max(id) + 1]

  value <- mean(c(a,b))

  pred <- rep(value, length(id))

  return(pred)

}

impKNPTS<-function(var, id, future = TRUE){

  npred <- length(id)

  #dejo el crossvalidation para mas tarde... de momento escojo un valor cualquiera de k
  #cvres <- cvknnvar(var.train = traincv, var.test = testcv, h = n, npred = n, kmax = 5, pond = FALSE)

  # k <- cvres['k'][which(cvres['DTW'] == min(cvres['DTW']))]
  #
  # dvar <- var[1:(min(id) - 1)]
  # pred <- predict.knn(datavar = dvar, kneighbors = k, npred = n, pond = FALSE)

  distknn <- distance_knn(datavar = var, id = id, h = 3, npred = npred, dist = "Euclidean", future = TRUE)

  kn <- kneighbors(distknn, k = 3)

  pred <- predictknn(datavar = var, kneighbors = kn, npred = npred, pond = FALSE)

  return(pred)

}

impRangesMean <- function(id, ranges){

  pred <- rep(mean(ranges), length(id))

  return(pred)

}




#
# impRangesMaxMin <- function(data, ranges, ind, listout){
#
#   for (i in ind){
#
#     data[[names(listout)[i]]][listout[[i]]] <- ifelse(data[[names(listout)[i]]][listout[[i]]] < ranges[[names(listout)[i]]][1], ranges[[names(listout)[i]]][1],
#                                                       ifelse(data[[names(listout)[i]]][listout[[i]]] > ranges[[names(listout)[i]]][2], ranges[[names(listout)[i]]][2], 'error'))
#
#   }
#
#   return(data)
#
# }
#
