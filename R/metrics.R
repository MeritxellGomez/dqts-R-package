# Completeness ------------------------------------------------------------

Completeness<-function(data){

  nr<-nrow(data)
  nc<-ncol(data)
  ncl<-nr*nc
  ic<-length(which(is.na(data)))
  comp<-(1-ic/ncl)*100

  return(comp)
}

CompletenessObservations<- function(data){

  a<-apply(data, 1, function(x) length(which(is.na(x))))
  nr<-nrow(data)
  nic<-length(which(a==ncol(data)))
  compobv=(1-nic/nr)*100

  if(compobv!=100) warning('One or more rows may be empty.')

  return(compobv)
}

CompletenessVariables<- function(data){

  a<-apply(data, 2, function(x) length(which(is.na(x))))
  nc<-ncol(data)
  nic<-length(which(a==nrow(data)))
  compvar=(1-nic/nc)*100

  if(compvar!=100) warning('One or more columns may be empty')

  return(compvar)
}


# Uniqueness --------------------------------------------------------------

# Uniqueness <- function(data){
#
#
# }

TimeUniqueness <- function(data, columnDate){

  if(is.numeric(columnDate)==FALSE) stop('columnDate may be numeric')

  length(unique(data[,columnDate]))*100 / length(data[,columnDate])

}


# Range -------------------------------------------------------------------

generateRangeData <- function(data){

  mysample <- data[sample(nrow(data), round(0.4*nrow(data))),]

  mins <- lapply(mysample, min)

  maxs <- lapply(mysample, max)

  df<- rbind(data.frame(mins), data.frame(maxs))

  return(df)
}

Range<-function(data, ranges=NULL){

  if(is.null(ranges)){

    ranges <- generateRangeData(data)

  }

  check<-list()
  for (i in 1:ncol(data)){
    if(is.numeric(data[,i])){
      auxcheck<-(length(which(data[,i]>=ranges[1,i] & data[,i]<=ranges[2,i])))*100/length(data[,i])
      check<-c(check, auxcheck) #lista de porcentajes de valores estan dentro del rango en cada variable
    }
  }
  return(sum(as.numeric(check))/length(check))
}



# Consistency -------------------------------------------------------------

consistent<-function(variable){

  mtemp<-mean(na.omit(variable))
  sdtemp<-sd(na.omit(variable))

  m<-mtemp-1.28*sdtemp
  M<-mtemp+1.28*sdtemp

  lower<-which(variable<m)
  upper<-which(variable>M)

  incons<-length(lower)+length(upper)
  return((1-(incons/length(variable)))*100)
}

Consistency<-function(data){
  co<-apply(as.matrix(data[,sapply(data, is.numeric)]), 2, consistent)
  return(sum(as.numeric(co))/length(co))
}



# Typicality --------------------------------------------------------------

typical<-function(variable){

  mtemp<-mean(na.omit(variable))
  sdtemp<-sd(na.omit(variable))

  m<-mtemp-1.96*sdtemp
  M<-mtemp+1.96*sdtemp

  lower<-which(variable<m)
  upper<-which(variable>M)

  atip<-length(lower)+length(upper)

  return((1-(atip/length(variable)))*100)
}

Typicality<-function(data){ #aplica función atípicos a todas las variables (columnas) de data
  at<-apply(as.matrix(data[,sapply(data, is.numeric)]), 2, typical)
  return(sum(as.numeric(at))/length(at)) #hace la media de todos los atipicos de cada variable
}


# Moderation --------------------------------------------------------------

moderate<-function(variable){

  mtemp<-mean(na.omit(variable))
  sdtemp<-sd(na.omit(variable))

  m<-mtemp-2.59*sdtemp
  M<-mtemp+2.59*sdtemp

  lower<-which(variable<m)
  upper<-which(variable>M)

  extr<-length(lower)+length(upper)

  return((1-(extr/length(variable)))*100)
}

Moderation<-function(data){ #aplica función extremos a todas las variables (columnas) de data
  moder<-apply(as.matrix(data[,sapply(data, is.numeric)]), 2, moderate)
  return(sum(as.numeric(moder))/length(moder)) #hace la media de todos los extemos de cada variable
}


# Timeliness --------------------------------------------------------------

Timeliness<-function(data, columnDate, maxdif){

  dif<-diff(data[,columnDate])

  outdif<-length(which(dif>maxdif))

  return((1-(outdif/length(dif)))*100)
}


# Conformity --------------------------------------------------------------

Conformity<-function(data, dataref=NULL){

  if(is.null(dataref)){conformity<-0}
  if(is.null(dataref)) warning('Reference data frame should be given')

  formats <- lapply(data, class)
  formatsref <- lapply(dataref, class)

  if(identical(formats,formatsref)==FALSE) warning('The variable formats are not correct')

  identicals <- length(which(unlist(formats) == unlist(formatsref)))

  conformity <- identicals/length(unlist(formats))*100

  return(conformity)
}


# Quality -----------------------------------------------------------------

quality<-function(data, columnDate, maxdif, dataref, ranges, weights){

  if(is.null(weights)){
    weights<-c(rep(0.1,10))
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

  time<-Timeliness(data,columnDate, maxdif)

  conf<-Conformity(data, dataref)

  quality<-(w[1]*comp + w[2]*compobv + w[3]*compvar +
              w[4]*tuni + w[5]*range + w[6]*cons +
              w[7]*typ + w[8]*mod + w[9]*time + w[10]*conf)

  return(list(Completeness = comp, CompletenessObservations = compobv,
              CompletenessVariables = compvar, TimeUniqueness = tuni,
              Range = range, Consistency = cons, Typicality = typ,
              Moderation = mod, Timeliness = time, Conformity = conf,
              DataQuality = quality))

}

