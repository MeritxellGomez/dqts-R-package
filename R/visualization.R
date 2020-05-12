qualityplot <- function(dataquality, totalquality = FALSE){

  if(totalquality==FALSE){dataquality<-dataquality[-length(dataquality)]}

  df.quality <- data.frame(id=seq(1,length(dataquality)),
                           metric = names(dataquality),
                           score = unlist(dataquality))

  library(ggplot2)
  g <- ggplot(data=df.quality, aes(x=metric, y=score)) +
    geom_bar(stat="identity", width = 0.4) +
    coord_cartesian(xlim = c(NA, NA), ylim = c(65,100)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust=0.9, vjust = 0.2, size = 15), axis.title.x = element_blank())

  return(g)
}
