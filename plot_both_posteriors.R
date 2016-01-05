library(ggplot2)
library(gridExtra)
library(XML)
library(plyr)
library(grid)


plot_posterior_distr <- function(limits, param_names, p_values_final_bi, p_values_final_tri){
  
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
        pltList[[k]] <-ggplot(p_values_final_bi, aes_string(x=param_names[i], weight=param_names[ncol(p_values_final_bi)])) + geom_density(fill="red", adjust=1.5/2, alpha=0.25) + ggtitle(param_names[i]) +
          geom_density(data=p_values_final_tri, aes_string(x=param_names[i], weight=param_names[ncol(p_values_final_tri)]), fill='blue', adjust=1.5/2, alpha=0.25)+
          xlim(a[i],b[i])+
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
      } else if (i > j){
        pltList[[k]] <-ggplot(p_values_final_bi, aes_string(x = param_names[i], y = param_names[j], weight=param_names[ncol(p_values_final_bi)])) + xlim(a[i],b[i])+ ylim(a[j],b[j])+
          stat_density2d(colour="red", alpha=0.7)+
          theme(axis.line=element_blank(),
                axis.text.x=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                legend.position="none",
                panel.grid.minor=element_blank(),
                plot.background=element_blank(),
                plot.margin=unit(c(0,0,-0.5,0), "lines"))
      }else if (i < j){
        pltList[[k]] <-ggplot(p_values_final_tri, aes_string(x = param_names[i], y = param_names[j], weight=param_names[ncol(p_values_final_tri)])) + xlim(a[i],b[i])+ ylim(a[j],b[j])+
          stat_density2d(colour="blue", alpha=0.7)+
          theme(axis.line=element_blank(),
                axis.text.x=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                legend.position="none",
                panel.grid.minor=element_blank(),
                plot.background=element_blank(),
                plot.margin=unit(c(0,0,-0.5,0), "lines"))
      }
      
    }
  png('posterior_comparison2.png', width = 980, height = 980)
  do.call("grid.arrange", pltList)
  dev.off()
}

p_values_final_asym = read.table("results_cl_asym/Parameter_values_final.txt")
p_weights_final_asym = read.table("results_cl_asym/Parameter_weights_final.txt")
p_values_final_asym$param_weights <- unlist(p_weights_final_asym)

p_values_final_results_rna = read.table("results_cl_rna/Parameter_values_final.txt")
p_weights_final_results_rna = read.table("results_cl_rna/Parameter_weights_final.txt")
p_values_final_results_rna$param_weights <- unlist(p_weights_final_results_rna)

doc = xmlInternalTreeParse("input_file_cl_rna.xml")
top = xmlRoot(doc)
df <- xmlToDataFrame(top[["parameters"]])
lim <- df[-1, 3:4]
limits <- do.call(cbind, lapply(df[-1, 3:4], as.vector))
param_nam <- do.call(cbind, lapply(df[-1,1], as.character))
param_names <- c(param_nam,"weights")
colnames(p_values_final_asym) = c(param_nam,"weights")
colnames(p_values_final_results_rna) = c(param_nam,"weights")
plot_posterior_distr(limits, param_names, p_values_final_asym, p_values_final_results_rna)


