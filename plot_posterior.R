library(ggplot2)
library(gridExtra)
library(XML)
library(plyr)
library(grid)

plot_posterior_distr <- function(limits, param_names, p_values_final){

  numb_params = length(param_names)-1
  a=as.numeric(limits[,1])
  b=as.numeric(limits[,2])
  pltList <- list()
  k=0
  for(i in 1:numb_params)
    for(j in 1:numb_params){
      k=k+1
      if(i==j){
      print(a[i])
      print(b[i])
        pltList[[k]] <-ggplot(p_values_final, aes_string(x=param_names[i], weight=param_names[ncol(p_values_final)])) + geom_density(fill="grey") + xlim(a[i],b[i])+ ggtitle(param_names[i]) +
          theme(axis.line=element_blank(),
                plot.title=element_text(size=8, hjust=0,lineheight=0),
                axis.text.x=element_text(size=6,angle = 90, vjust=0,hjust=1.2),
                axis.text.y=element_text(size=6),
                axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                legend.position="none",
                panel.grid.minor=element_blank(),
                plot.background=element_blank(),
                plot.margin=unit(c(0,0,-0.5,0), "lines"))
      }else{
        pltList[[k]] <-ggplot(p_values_final, aes_string(x = param_names[i], y = param_names[j], weight=param_names[ncol(p_values_final)])) + xlim(a[i],b[i])+ ylim(a[j],b[j])+
          stat_density2d(aes(alpha=..level.., fill=..level.., weight=weights),
                         size=2, bins=50, geom="polygon") +
          scale_fill_gradient(low = "yellow", high = "red") +
          scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
          geom_density2d(colour="black", bins=10)+
          theme(axis.line=element_blank(),
                axis.text.x=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                legend.position="none",
                #panel.background=element_blank(),
                #panel.border=element_blank(),
                panel.grid.minor=element_blank(),
                plot.background=element_blank(),
                plot.margin=unit(c(0,0,-0.5,0), "lines"))
      }
    }
   pdf('posterior.pdf')
   do.call("grid.arrange", pltList)
   dev.off()
}
#Set path to data and weights here:
p_values_final = read.table("results_txt_files/Parameter_values_final.txt")
p_weights_final = read.table("results_txt_files/Parameter_weights_final.txt")

#The first parameter is ignored. If it shouldn't be removed then comment out the following line.
p_values_final <- subset(p_values_final, select = -p_values_final[,1] )

p_values_final$param_weights <- unlist(p_weights_final)
doc = xmlInternalTreeParse("input_file.xml")
top = xmlRoot(doc)
df <- xmlToDataFrame(top[["parameters"]])

#The first parameter is ignored. If it shouldn't be, remove the -1 in the following three lines.
lim <- df[-1, 3:4]
limits <- do.call(cbind, lapply(df[-1, 3:4], as.vector))
param_nam <- do.call(cbind, lapply(df[-1,1], as.character))

param_names <- c(param_nam,"weights")
colnames(p_values_final) = c(param_nam,"weights")

plot_posterior_distr(limits, param_names, p_values_final)
