segwdw <- function(data, ind){

  n <- nrow(data)
  l_i <- length(ind)

  dfs <- list()

  for(i in 1:l_i){

    dfs[[i]] <- data[ind[[i]],]

  }

  return(dfs)

}

#windows caret

windowpartitions <- function(n_data, initialWindow, skip, fixedWindow){ #sustiuye a wdwind

  partitions <- caret::createTimeSlices(c(1:n_data), initialWindow = initialWindow, skip = skip, fixedWindow = fixedWindow)

  return(partitions$train)

}





