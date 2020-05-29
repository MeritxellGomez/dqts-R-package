

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




