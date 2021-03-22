
#windows caret

windowpartitions <- function(n_data, initialWindow, skip, fixedWindow){ #sustiuye a wdwind

  partitions <- caret::createTimeSlices(c(1:n_data), initialWindow = initialWindow, skip = skip, fixedWindow = fixedWindow)

  return(partitions$train)

}





