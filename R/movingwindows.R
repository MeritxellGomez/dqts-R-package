
#data quality
wdwind <- function(n_data, cte, fixed, nint, by){

  n <- trunc(n_data/nint)

  indexs <- vector(mode="list", length = nint)

  if(cte & fixed){

    #ventanas fijas (intervalos)

    n_ini<-1
    n_fin <- n

    for (i in 1:nint){

      aux <- c(n_ini : n_fin)

      indexs[[i]] <- aux

      n_ini <- n_fin+1
      n_fin <- n_fin +n

    }

  }else{

    n_ini <- 1
    n_fin <- n_data - ((nint-1) * by)

    for (i in 1:nint){

      aux <- c(n_ini : n_fin)

      indexs[[i]] <- aux

      n_fin <- n_fin + by
      if(cte){
        n_ini <- n_ini + by
      }

    }

  }

  return(indexs)

}


segwdw <- function(data, ind){

  #funcion que utilizando la anterior wdwind devuelva tantos dataframes como particiones se hayan hecho
  #estos dataframes tendran elementos cuyos indices son el output de la funcion wdwind

  n <- nrow(data)
  l_i <- length(ind)

  dfs <- list()

  for(i in 1:l_i){

    dfs[[i]] <- data[ind[[i]],]

  }

  return(dfs)

}

#imputation

d_euclidean <- function(vec_x, vec_y){

  d <- sqrt(sum((vec_x - vec_y)^2))

  return(d)

}


#funcion que de un data frame salido de la funcion distance_knn (ordenado), escoger los k más cercanos
kneighbors <- function(distk, k){

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

  #built dataframe to be written
  dfdist <- data.frame(initial.value = integer(), final.value = integer(), distances = double(), stringsAsFactors = FALSE)


  #look for the distances with previous data
  initial1_f <- firstna - h - npred

  for (i in 1:initial1_f){

    int <- datavar[i:(i+h-1)]

    d_aux <- d_euclidean(int, h_data)

    dfdist[i,] <- data.frame(initial.value = i, final.value = i+h-1, distances = d_aux)

  }

  #if future == TRUE, look for the distances with future data
  if(future){

    initial2_0 <- max(idna) + 1
    initial2_f <- length(datavar) - h - npred + 1
    j <- 1

    for(i in initial2_0:initial2_f){

      int <- datavar[i:(i+h-1)]

      d_aux <- d_euclidean(int, h_data)

      dfdist[initial1_f+j,] <- data.frame(initial.value = i, final.value = i+h-1, distances = d_aux)

      j<-j+1

    }

  }


  library(dplyr)
  dfdist <- dfdist %>% arrange(distances)

  return(dfdist)

}


predictknn<-function(datavar, kneighbors, npred, pond){

  nearest.neighbors <- kneighbors[['final.value']]

  k<-nrow(kneighbors)

  m <- matrix(, nrow = k, ncol = npred)

  #align the values of the neighbors
  for (i in 1:k){
    m[i,] <- datavar[(nearest.neighbors[i]+1) : (nearest.neighbors[i]+npred)]
  }

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


