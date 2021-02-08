# Completeness ------------------------------------------------------------

Completeness<-function(data){

  nr<-nrow(data)
  nc<-ncol(data)
  ncl<-nr*nc
  ic<-length(which(is.na(data)))
  comp<-1-ic/ncl

  return(comp)
}

CompletenessObservations<- function(data){

  a<-apply(data, 1, function(x) length(which(is.na(x))))
  nr<-nrow(data)
  nic<-length(which(a==ncol(data)))
  compobv=1-nic/nr

  if(compobv!=1) warning('One or more rows may be empty.')

  return(compobv)
}

CompletenessVariables<- function(data){

  a<-apply(data, 2, function(x) length(which(is.na(x))))
  nc<-ncol(data)
  nic<-length(which(a==nrow(data)))
  compvar=1-nic/nc

  if(compvar!=1) warning('One or more columns may be empty')

  return(compvar)
}


# Uniqueness --------------------------------------------------------------

# Uniqueness <- function(data){
# create uniqueness function to assess the uniqueness in other variables. Not necessary in time series.
#
# }

TimeUniqueness <- function(data, columnDate){

  if(is.numeric(columnDate)==FALSE) stop('columnDate may be numeric')

  length(unique(data[,columnDate])) / length(data[,columnDate])

}


# Range -------------------------------------------------------------------

generateRangeData <- function(data){

  mysample <- data[sample(nrow(data), round(0.4*nrow(data))),]

  mysample[, sapply(mysample, is.factor)] <- sapply(mysample[,sapply(mysample, is.factor)], as.character)

  mins <- lapply(mysample, function(x) min(x, na.rm = TRUE))

  maxs <- lapply(mysample, function(x) max(x, na.rm = TRUE))

  df<- rbind(data.frame(mins), data.frame(maxs))

  return(df)
}

isoutofrange <- function(data, ranges){

  check<-list()
  for (i in 1:ncol(data)){
    if(is.numeric(data[,i])){

      aux<-which(data[,i] < ranges[1,i] | data[,i] > ranges[2,i])
      check[[colnames(data)[i]]] <- aux
    }
  }
  return(check)
}

Range<-function(data, ranges){

  out <- isoutofrange(data, ranges)
  totalout <- length(unlist(out))
  n<- length(which(!is.na(data)))

  #ratio to different NA elements
  return(1 - (totalout / n))
}



# Consistency -------------------------------------------------------------

consistent<-function(variable){

  n<-length(variable)
  variable <- variable[1:trunc(n/3)]

  mtemp<-mean(na.omit(variable))
  sdtemp<-sd(na.omit(variable))

  m<-mtemp-1.28*sdtemp
  M<-mtemp+1.28*sdtemp

  lower<-which(variable<m)
  upper<-which(variable>M)

  incons<-length(lower)+length(upper)
  return(1-(incons/length(variable)))
}

Consistency<-function(data){
  co<-apply(as.matrix(data[,sapply(data, is.numeric)]), 2, consistent)
  return(sum(as.numeric(co))/length(co))
}



# Typicality --------------------------------------------------------------

typical<-function(variable){

  n<-length(variable)
  variable <- variable[1:trunc(n/3)]

  mtemp<-mean(na.omit(variable))
  sdtemp<-sd(na.omit(variable))

  m<-mtemp-1.96*sdtemp
  M<-mtemp+1.96*sdtemp

  lower<-which(variable<m)
  upper<-which(variable>M)

  atip<-length(lower)+length(upper)

  return(1-(atip/length(variable)))
}

Typicality<-function(data){ #aplica función atípicos a todas las variables (columnas) de data
  at<-apply(as.matrix(data[,sapply(data, is.numeric)]), 2, typical)
  return(sum(as.numeric(at))/length(at)) #hace la media de todos los atipicos de cada variable
}


# Moderation --------------------------------------------------------------

moderate<-function(variable){

  n<-length(variable)
  variable <- variable[1:trunc(n/3)]

  mtemp<-mean(na.omit(variable))
  sdtemp<-sd(na.omit(variable))

  m<-mtemp-2.59*sdtemp
  M<-mtemp+2.59*sdtemp

  lower<-which(variable<m)
  upper<-which(variable>M)

  extr<-length(lower)+length(upper)

  return(1-(extr/length(variable)))
}

Moderation<-function(data){ #aplica función extremos a todas las variables (columnas) de data
  moder<-apply(as.matrix(data[,sapply(data, is.numeric)]), 2, moderate)
  return(sum(as.numeric(moder))/length(moder)) #hace la media de todos los extemos de cada variable
}


# Timeliness --------------------------------------------------------------

Timeliness<-function(data, columnDate, maxdif, units){
  #se contempla 'secs', 'mins', 'hours', 'days', 'months'

  if(units == 'months'){
    maxdif <- 31
    units2 <- 'days'
  }else{
    units2 <- units
  }

  dif <- diff(data[,columnDate])

  dif <- as.numeric(dif)

  outdif <- length(which(dif > maxdif))

  if(outdif == 0){
    timeliness <- 1
  }else{
    pos <- which(dif > maxdif)
    loss.start <- data[pos,columnDate]
    loss.finish <- data[pos+1,columnDate]

    waiting.time <- difftime(loss.finish, loss.start, units = units2)

    if(units == 'months'){
      missing.amount <- round(((as.numeric(abs(waiting.time)))/maxdif)-1)
    }else if(units == 'days'){
      missing.amount <- trunc(((as.numeric(abs(waiting.time)))/maxdif))
    }
    else{
      missing.amount <- trunc(((as.numeric(abs(waiting.time)))/maxdif)-1)
    }

    totaltimes <- sum(missing.amount) + length(data[,columnDate])

    timeliness <- length(data[,columnDate]) / totaltimes
  }

  return(timeliness)
}



# Conformity --------------------------------------------------------------
generateReferenceData <- function(data){

  mysample <- data[sample(nrow(data), round(0.4*nrow(data))),]

  types <- lapply(mysample, class)
  types <- lapply(types, function(x) if(length(x) != 1){x <- paste(x, collapse = " ")}else{x <- x})
  df<- data.frame(types, stringsAsFactors = FALSE)

  colnames(df) <- colnames(mysample)

  return(df)

}

Formats<-function(data, dataref){

  formats <- lapply(data, class)
  formats <- lapply(formats, function(x) if(length(x) != 1){x <- paste(x, collapse = " ")}else{x <- x})
  formats <- data.frame(formats, stringsAsFactors = FALSE)

  if(identical(formats[1,],dataref[1,])==FALSE) warning('The variable formats are not correct')

  identicals <- length(which(formats[1,] == dataref[1,]))

  formatsvalue <- identicals/ncol(formats)

  return(formatsvalue)

}

Names <- function(data, dataref){

  col <- colnames(data)
  colref <- colnames(dataref)

  if(identical(col,colref)==FALSE) warning('The variable names are not correct')

  identicals <- length(which(unlist(col) == unlist(colref)))

  namesvalue <- identicals/length(unlist(col))

  return(namesvalue)

}


# Quality -----------------------------------------------------------------

quality<-function(data, columnDate, maxdif, units, dataref, ranges=NULL, weights=NULL){

  if(is.null(weights)){
    weights<-c(rep((1/11),11))
  }

  w<-weights

  comp<-Completeness(data)
  compobv<-CompletenessObservations(data)
  compvar<-CompletenessVariables(data)

  tuni<-TimeUniqueness(data,columnDate)

  range<-Range(data, ranges)

  cons<-Consistency(data)
  typ<-Typicality(data)
  mod<-Moderation(data)

  time<-Timeliness(data,columnDate, maxdif, units)

  form<-Formats(data, dataref)
  nam<-Names(data,dataref)

  quality<-(w[1]*comp + w[2]*compobv + w[3]*compvar +
              w[4]*tuni + w[5]*range + w[6]*cons +
              w[7]*typ + w[8]*mod + w[9]*time + w[10]*form + w[11]*nam)

  return(data.frame(InitialDate = data[1,columnDate], FinalDate = data[nrow(data), columnDate], Completeness = comp, CompletenessObservations = compobv,
              CompletenessVariables = compvar, TimeUniqueness = tuni,
              Range = range, Consistency = cons, Typicality = typ,
              Moderation = mod, Timeliness = time, Formats = form,
              Names = nam, DataQuality = quality))

}

