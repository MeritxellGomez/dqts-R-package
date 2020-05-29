
#barplot para salida de DQ cuando no hay ventanas. Calidad TOTAL
qualitybarplot <- function(DQ, totalquality = FALSE){

  if(totalquality==FALSE){DQ<-DQ[-length(DQ)]}

  df.quality <- data.frame(id=seq(1,length(DQ)),
                           metric = names(DQ),
                           score = unlist(DQ), stringsAsFactors = FALSE)

  library(ggplot2)
  g <- ggplot(data=df.quality, aes(x=metric, y=score)) +
    geom_bar(stat="identity", width = 0.4) +
    # coord_cartesian(xlim = c(NA, NA), ylim = c(65,100)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust=0.9, vjust = 0.2, size = 15), axis.title.x = element_blank())

  return(g)
}

#gráfico cuando hay ventanas en DQ
movDQplot <- function(movingDQ, totalquality=FALSE){

  library(reshape2)

  d <- melt(movingDQ, id.vars=c("InitialDate", "FinalDate"))

  ggplot(d, aes(FinalDate,value, col=variable)) +
    geom_line() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))
}


