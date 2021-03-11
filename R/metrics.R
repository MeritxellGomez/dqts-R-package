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

TimeUniqueness <- function(data, var_time_name){

  length(unique(data[[var_time_name]])) / length(data[[var_time_name]])

}


# Range -------------------------------------------------------------------

generateRangeData <- function(data){

  mysample <- data[sample(nrow(data), round(0.3*nrow(data))),]

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



# Normality  --------------------------------------------------------------

isoutofnormality<-function(data, metric, mean, sd){

  z <- ifelse(metric == 'Consistency', qnorm(0.9),
              ifelse(metric == 'Typicality', qnorm(0.975),
                     ifelse(metric == 'Moderation', qnorm(0.995), stop('Incorrect name of metric'))))

  check <- list()

  for (i in 1:ncol(data)){

    lower <- mean[i] - z*sd[i]
    upper <- mean[i] + z*sd[i]

    aux<-which(data[,i] < lower | data[,i] > upper)
    check[[colnames(data)[i]]] <- aux
  }

  return(check)

}


outofnormality <- function(data){

  set.seed(111)
  numericdata <- data %>% dplyr::select_if(is.numeric)

  if(nrow(numericdata) > 300){
    sampledata <- numericdata[1:(nrow(numericdata)/3),] %>%
      as.data.frame() %>%
      dplyr::sample_n(., size = trunc(0.3*nrow(data)))
    colnames(sampledata) <- colnames(numericdata)

    if(nrow(sampledata) > 4999){
      sampledata1 <- as.data.frame(sampledata[1:4999,])
      colnames(sampledata1) <- colnames(sampledata)
      sampledata <- sampledata1
    }

  }else{
    sampledata <- numericdata[1:(nrow(numericdata)/3),] %>% as.data.frame()
    colnames(sampledata) <- colnames(numericdata)
  }

  #remove columns where ALL values are NA
  #sampledata <- sampledata[,colSums(is.na(sampledata))<nrow(sampledata)]

  #remove columns with ALL values identical
  sampledata <- sampledata[vapply(sampledata, function(x) length(unique(na.omit(x))) > 1, logical(1L))]

  #pvalues by variables
  pvalues <- apply(sampledata, 2, function(x) shapiro.test(x)$p.value)

  if(length(which(pvalues > 0.05)) == 0){
    out_normality <- NULL
  }else{

    namevars <- names(which(pvalues>0.05))

    mean_normalvars <- apply(sampledata[namevars], 2, function(x) mean(x, na.rm = TRUE))
    sd_normalvars <- apply(sampledata[namevars], 2, function(x) sd(x, na.rm = TRUE))


    out_consistency <- isoutofnormality(data[namevars], metric = 'Consistency', mean = mean_normalvars, sd = sd_normalvars)
    out_typicality <- isoutofnormality(data[namevars], metric = 'Typicality', mean = mean_normalvars, sd = sd_normalvars)
    out_moderation <- isoutofnormality(data[namevars], metric = 'Moderation', mean = mean_normalvars, sd = sd_normalvars)

    out_normality <- list(Consistency = out_consistency, Typicality = out_typicality, Moderation = out_moderation)

  }

  return(out_normality)

}


Normality <- function(data, outnormality, metric, group = TRUE){

  out_metric <- outnormality[[metric]]

  p <- ifelse(metric == 'Consistency', 0.2, ifelse(metric == 'Typicality', 0.05, 0.01))

  normbyvars <- list()
  for(i in 1:length(out_metric)){

    aux <- length(out_metric[[i]]) / length(which(!is.na(data[[names(out_metric)[i]]])))

    aux_out <- aux - p

    normbyvars[i] <- 1 - aux_out

  }

  names(normbyvars) <- names(out_metric)

  if(group){
    normality <- (mean(unlist(normbyvars)))
  }else{
    normality <- normbyvars
  }

  return(normality)

}


# Timeliness --------------------------------------------------------------

generateMaxDif <- function(data, var_time_name){

  timevar <- data[[var_time_name]]
  timevarsample <- sample(timevar, size = trunc(0.3*length(timevar)))

  diffs <- diff(timevarsample)
  uniq <- unique(diffs)
  maxdif <- uniq[which.max(tabulate(match(diffs, uniq)))]

  return(maxdif)

}

Timeliness<-function(data, var_time_name, maxdif, units){
  #se contempla 'secs', 'mins', 'hours', 'days', 'months'

  if(units == 'months'){
    maxdif <- 31
    units2 <- 'days'
  }else{
    units2 <- units
  }

  date_vec <- data[[var_time_name]]
  n <- length(date_vec)
  dif <- difftime(date_vec[2:n], date_vec[1:(n-1)], units = units)

  outdif <- length(which(dif > maxdif))

  if(outdif == 0){
    timeliness <- 1
  }else{
    pos <- which(dif > maxdif)
    loss.start <- data[[var_time_name]][pos]
    loss.finish <- data[[var_time_name]][pos+1]

    waiting.time <- difftime(loss.finish, loss.start, units = units2)

    if(units == 'months'){
      missing.amount <- round(((as.numeric(abs(waiting.time)))/maxdif)-1)
    }else if(units == 'days'){
      missing.amount <- trunc(((as.numeric(abs(waiting.time)))/maxdif))
    }
    else{
      missing.amount <- trunc(((as.numeric(abs(waiting.time)))/maxdif)-1)
    }

    totaltimes <- sum(missing.amount) + length(data[[var_time_name]])

    timeliness <- length(data[[var_time_name]]) / totaltimes
  }

  return(timeliness)
}



# Conformity --------------------------------------------------------------
generateReferenceData <- function(data){

  mysample <- data[sample(nrow(data), round(0.3*nrow(data))),]

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

quality<-function(data, var_time_name, maxdif, units, dataref, ranges, weights){

  w<-weights

  comp<-Completeness(data)

  if(comp == 1){
    compobv <- 1
    compvar <- 1
  }else{
    compobv<-CompletenessObservations(data)
    compvar<-CompletenessVariables(data)
  }

  tuni<-TimeUniqueness(data,var_time_name)

  range<-Range(data, ranges)

  norm <- outofnormality(data)

  if(is.null(norm)){
    cons <- 0
    typ <- 0
    mod <- 0
    w[6:8] <- 0
    #meter funcion de recalcular los otros pesos guardando proporcion
    w[c(1:5,9:11)] <- 1/8
  }else{
    cons <- Normality(data, outnormality = norm, metric = 'Consistency')
    typ <- Normality(data, outnormality = norm, metric = 'Typicality')
    mod <- Normality(data, outnormality = norm, metric = 'Moderation')
  }

  time<-Timeliness(data,var_time_name, maxdif, units)

  form<-Formats(data, dataref)
  nam<-Names(data,dataref)

  quality<-(w[1]*comp + w[2]*compobv + w[3]*compvar +
              w[4]*tuni + w[5]*range + w[6]*cons +
              w[7]*typ + w[8]*mod + w[9]*time + w[10]*form + w[11]*nam)

  return(data.frame(InitialDate = data[[var_time_name]][1], FinalDate = data[[var_time_name]][nrow(data)],
                    Completeness = comp, CompletenessObservations = compobv,
                    CompletenessVariables = compvar, TimeUniqueness = tuni,
                    Range = range, Consistency = cons, Typicality = typ,
                    Moderation = mod, Timeliness = time, Formats = form,
                    Names = nam, DataQuality = quality))

}

