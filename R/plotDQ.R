#' Plot data quality metrics
#'
#' Takes a data quality data frame and plot the values of the metrics
#'
#' @param totalquality Logical argument. If TRUE the global data quality is added with the other metrics
#' @param DQ A data frame with quality metrics as columns
#'
#' @return Plot of the quality metric values
#' @export
dqplot <- function(DQ, totalquality = FALSE, normal = TRUE){

  if(nrow(DQ) == 1){

    qualitybarplot(DQ, totalquality, normal)

  }else{

    movDQplot(DQ, totalquality, normal)

  }

}


qualitybarplot <- function(DQ, totalquality, normal){

  library(dplyr)

  if(totalquality == FALSE){DQ <- DQ %>%  select(-DataQuality)}
  if(normal == FALSE){DQ <- DQ %>% select(-c(Consistency, Typicality, Moderation))}

  #delete date variables
  DQ <- DQ %>% select(-c(InitialDate, FinalDate))

  df.quality <- data.frame(id=seq(1,length(DQ)),
                           metric = names(DQ),
                           score = round(unlist(DQ),3), stringsAsFactors = FALSE)

  library(ggplot2)
  g <- ggplot(data=df.quality, aes(x=metric, y=score)) +
    geom_bar(stat="identity", width = 0.4) +
    geom_text(aes(label=score), vjust = 1.6, size = 3, color="white")+
    # coord_cartesian(xlim = c(NA, NA), ylim = c(65,100)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust=0.9, vjust = 0.2, size = 15), axis.title.x = element_blank())

  return(g)
}

movDQplot <- function(movingDQ, totalquality, normal){

  library(reshape2)

  library(dplyr)

  if(totalquality == FALSE){movingDQ <- movingDQ %>%  select(-DataQuality)}
  if(normal == FALSE){movingDQ <- movingDQ %>% select(-c(Consistency, Typicality, Moderation))}

  d <- melt(movingDQ, id.vars=c("InitialDate", "FinalDate"))

  library(ggplot2)

  ggplot(d, aes(FinalDate,value, col=variable)) +
    geom_line() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))
}

