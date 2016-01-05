packages = c("flowCore", "flowViz", "ggplot2", "plyr", "scales", "reshape2", "RColorBrewer","ggExtra","gridExtra")

lapply(packages, require, character.only = TRUE)
sample.info = read.csv("samples.csv", header = T, stringsAsFactors = F)
#sample.info <- sample.info[1:24,]
sample.info[is.na(sample.info)] <- 1.000
sample.info$sample = as.character(sample.info$sample)
sample.info$inducer = factor(sample.info$inducer, levels = c("atc",'iptg'))
sample.info$repeats = factor(sample.info$repeats, levels = c("1", "2", "3"))
samples = sample.info$sample
files = list.files(pattern = "*.fcs")

#truncTrans = truncateTransform(transformationId = "Truncate-transformation", a = 1)
#flowData = transform(flowData,`green` = truncTrans(`GFP`), `red` = truncTrans(`mCherry`))

extractFlowData = function(flowData, samples, sample.info) {
  require("plyr")
  data = vector("list", length(samples))
  names(data) = samples
  for (i in 1:length(samples)) {
    assign(samples[i], data.frame(exprs(flowData[[i]])))
    data[[i]] = get(samples[i])
  }
  rm(list = c(samples, "i"))
  data = ldply(data, data.frame)
  print(head(data))
  data$.id = factor(data$.id)
  data = merge(sample.info, 
               data,
               by.x = "sample",
               by.y = ".id")
}

flowData = read.flowSet(files = files, transformation = F)
colnames(flowData) = c("fscA", "fscH", "sscA", "sscH", "GFP","mCherry","Time")
sampleNames(flowData) = samples
data = extractFlowData(flowData, samples, sample.info)

#------------------------------------------------------------------------------------------

# Plot of fsc v ssc
#png(filename = "fscVssc.png", width = 7, height = 7, units = "in", res = 400)
#print(xyplot(`sscA` ~ `fscA`, data = flowData, smooth = FALSE))
#dev.off()
#data[,5:510] <- log(data[,5:10]+1)
data_atc <- subset(data, inducer=='atc') 
data_iptg <- subset(data, inducer=='iptg') 
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p <- ggplot(data_iptg, aes(x = GFP, y = mCherry, colour = repeats)) +
  geom_density2d() +
  scale_colour_manual(values=cbPalette)+
  facet_wrap( ~ concentration) +
  scale_x_log10(name   = "EGFP fluorescence, A.U.",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(name   = "mCherry fluorescence, A.U.",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw()
ggsave(file="density_plot_concentration_facets_iptg.png", p,width=9, height=9, dpi=300)

#----------------------------------------------------------------------------------------------------------
plot_density2d_marginals = function(conc){
  empty <- ggplot()+geom_point(aes(1,1), colour="white") +
    theme(                              
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
    )
  dens2 <- ggplot(subset(data,times==conc),aes(x=GFP,y=mCherry,colour=repeats)) + geom_density2d() + 
    scale_color_brewer(palette="Dark2")+
    scale_x_log10(name   = "GFP fluorescence, A.U.",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1,1e4)) +
    scale_y_log10(name   = "mCherry fluorescence, A.U.",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1,1e4)) + theme_bw() +theme(legend.position=c(0.05,0.15),plot.margin = unit(c(0,0,0,0),"lines"))
  theme0 <- function(...) theme( legend.position = "none",
                               panel.background = element_blank(),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.margin = unit(0,"null"),
                               axis.ticks = element_blank(),
                               axis.text.x = element_blank(),
                               axis.text.y = element_blank(),
                               axis.title.x = element_blank(),
                               axis.title.y = element_blank(),
                               axis.ticks.length = unit(0,"null"),
                               axis.ticks.margin = unit(0,"null"),
                               panel.border=element_rect(color=NA),...)
  #top, right, bottom, and left margins
  plot_top <- ggplot(subset(data,times==conc),aes(x=GFP,colour=repeats)) + 
    geom_density(alpha=0.5) + 
    scale_color_brewer(palette="Dark2")+
    scale_x_log10(name   = "GFP fluorescence, A.U.",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1,1e4)) +theme_bw() +theme0(plot.margin = unit(c(1,0,-0.5,1.55),"lines")) 
  plot_right <- ggplot(subset(data,times==conc),aes(x=mCherry,colour=repeats)) + 
    geom_density(alpha=0.5) + 
    coord_flip()  +
    scale_color_brewer(palette="Dark2")+
    scale_x_log10(name   = "GFP fluorescence, A.U.",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1,1e4)) +theme_bw() + theme0(plot.margin = unit(c(0,0,1.3,-0.5),"lines"))
  p_m <- grid.arrange(plot_top, empty, dens2, plot_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))

  g <- arrangeGrob(plot_top, empty, dens2, plot_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4),main=textGrob(paste('Toggle switch fluorescence densities, with ', conc ,' hours aTc induction',sep=""), vjust =1, gp = gpar(fontsize=14, fontface="bold", fontsize=14))) 
  ggsave(file=paste("density2d_marginals_atc_",conc,"_hours.pdf",sep=""), g,width=12, height=10, dpi=300)
}

plot_density2d_marginals("0.5")
#------------------------------------------------------------------------------------------
p <- ggplot(data_iptg, aes(x = mCherry, colour = repeats)) +
  geom_density() +
  facet_wrap( ~ concentration) +
  theme_bw()+
  scale_colour_manual(values=cbPalette)+
  scale_x_log10(name   = "mCherry fluorescence, A.U.",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) 
p <- p + labs(title = "mCherry fluorescence density with increasing IPTG concenrtations")
ggsave(file="mCherry_density_concentrations_iptg.png", p,width=9, height=9, dpi=300)
#---------------------------------------------------------------------------------------
data_iptg <- subset(data, inducer=="iptg")
p <- ggplot(data_iptg, aes(x = GFP, colour = as.factor(concentration))) +
  geom_density() +
  theme_bw()+
  scale_colour_grey()+
  #scale_colour_manual(values=cbPalette)+
  facet_wrap( ~ repeats) +
  #geom_vline(xintercept = 200)+
  scale_x_log10(name   = "mCherry fluorescence, A.U.",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))
  

ggsave(file="mCherry_density_repeats_all_concentrations_iptg__GFP_gr.png", p,width=15, height=7, dpi=500)
#------------------------------------------------------------------------------------------
#library(plyr)
#tabl <- ddply(data, .(concentration), nrow)

a <- subset(data, concentration=="0") 
a1 <- subset(a, repeats=="1") 
a2<- subset(a, repeats=="2") 
a3<- subset(a, repeats=="3") 
s1 <- length( which( a1[,10] > 200) )/nrow(a1)
w1 <- length( which( a2[,10] > 200) )/nrow(a2)
t1 <- length( which( a3[,10] > 200) )/nrow(a3)
#"0","0.0000001","0.0000006","0.000001","0.000006","0.00001","0.001","0.1"
b<- subset(data, concentration=="1e-07") 
b1<- subset(b, repeats=="1") 
b2<- subset(b, repeats=="2") 
b3<- subset(b, repeats=="3") 
s2 <- length( which( b1[,10] > 200) )/nrow(b1)
w2 <- length( which( b2[,10] > 200) )/nrow(b2)
t2 <- length( which( b3[,10] > 200) )/nrow(b3)

c<- subset(data, concentration=="6e-07") 
c1<- subset(c, repeats=="1") 
c2<- subset(c, repeats=="2") 
c3<- subset(c, repeats=="3") 
s3 <- length( which( c1[,10] > 200) )/nrow(c1)
w3 <- length( which( c2[,10] > 200) )/nrow(c2)
t3 <- length( which( c3[,10] > 200) )/nrow(c3)

d<- subset(data, concentration=="1e-06") 
d1<- subset(d, repeats=="1") 
d2<- subset(d, repeats=="2") 
d3<- subset(d, repeats=="3") 
s4 <- length( which( d1[,10] > 200) )/nrow(d1)
w4 <- length( which( d2[,10] > 200) )/nrow(d2)
t4 <- length( which( d3[,10] > 200) )/nrow(d3)

e<- subset(data, concentration=="6e-06") 
e1<- subset(e, repeats=="1") 
e2<- subset(e, repeats=="2") 
e3<- subset(e, repeats=="3") 
s5 <- length( which( e1[,10] > 200) )/nrow(e1)
w5 <- length( which( e2[,10] > 200) )/nrow(e2)
t5 <- length( which( e3[,10] > 200) )/nrow(e3)

f<- subset(data, concentration=="1e-05") 
f1<- subset(f, repeats=="1") 
f2<- subset(f, repeats=="2") 
f3<- subset(f, repeats=="3") 
s6 <- length( which( f1[,10] > 200) )/nrow(f1)
w6 <- length( which( f2[,10] > 200) )/nrow(f2)
t6 <- length( which( f3[,10] > 200) )/nrow(f3)

g<- subset(data, concentration=="0.001") 
g1<- subset(g, repeats=="1") 
g2<- subset(g, repeats=="2") 
g3<- subset(g, repeats=="3") 
s7 <- length( which( g1[,10] > 200) )/nrow(g1)
w7 <- length( which( g2[,10] > 200) )/nrow(g2)
t7 <- length( which( g3[,10] > 200) )/nrow(g3)

h<- subset(data, concentration=="0.1") 
h1<- subset(h, repeats=="1") 
h2<- subset(h, repeats=="2") 
h3<- subset(h, repeats=="3") 
s8 <- length( which( h1[,10] > 200) )/nrow(h1)
w8 <- length( which( h2[,10] > 200) )/nrow(h2)
t8 <- length( which( h3[,10] > 200) )/nrow(h3)

prop1 <- c(s1,s2,s3,s4,s5,s6,s7,s8,w1,w2,w3,w4,w5,w6,w7,w8,t1,t2,t3,t4,t5,t6,t7,t8)
concentrations <- c("0","0.0000001","0.0000006","0.000001","0.000006","0.00001","0.001","0.1","0","0.0000001","0.0000006","0.000001","0.000006","0.00001","0.001","0.1","0","0.0000001","0.0000006","0.000001","0.000006","0.00001","0.001","0.1")
#concentrations <- c("0","0.01","0.05","0.06","0.07","0.08","0.09","1","0","0.01","0.05","0.06","0.07","0.08","0.09","1","0","0.01","0.05","0.06","0.07","0.08","0.09","1")
repeats <-c("1","1","1","1","1","1","1","1","2","2","2","2","2","2","2","2","3","3","3","3","3","3","3","3")
test2 <- data.frame(concentrations, prop1,repeats)

test2$concentrations <- as.numeric(as.character(test2$concentrations))
pl <- ggplot(test2, aes(x=concentrations, y=prop1,colour=repeats)) + geom_point() +geom_path()+theme_bw() + scale_x_log10()+ylim(0,1) +xlab("mCherry concentration (M)") + ylab("proportion of cells ON")+ 
  scale_colour_manual(values = rev(brewer.pal(10,"Spectral")))
pl <- pl + labs(title = "Proportions of cells ON with increasing IPTG concentration")
ggsave(file="IPTG_proportion_ON_repeats.png", pl,width=12, height=8, dpi=300)

#------------------------------------------------------------------------------------------
df <- subset(data, repeats=="1")

ggplot(df, aes(x=GFP, y=mCherry, colour=as.factor(concentration))) + geom_density2d()

+ scale_x_log10(name   = "GFP fluorescence, A.U.",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1,1e4)) +
  scale_y_log10(name   = "mCherry fluorescence, A.U.",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1,1e4)) + theme_bw() +theme(legend.position="none",plot.margin = unit(c(0,0,0,0),"lines"))
#------------------------------------------------------------------------------------------
data_iptg <- subset(data, inducer=="atc")
df <- subset(data_iptg, repeats=="2")

empty <- ggplot()+geom_point(aes(1,1), colour="white") +
  theme(                              
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

#df <- subset(df, inducer=="iptg")
dens2 <- ggplot(df, aes(x=GFP, y=mCherry, colour=as.factor(concentration))) + geom_density2d()+ 
  scale_colour_grey()+
  #scale_colour_manual(values=cbPalette)+
  #scale_colour_manual(values = brewer.pal(10,"Spectral"))+
  #scale_color_brewer(palette="Spectral")+
  scale_x_log10(name   = "GFP fluorescence, A.U.",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1,1e4)) +
  scale_y_log10(name   = "mCherry fluorescence, A.U.",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1,1e6)) + theme_bw() +
  theme(legend.position=c(0.1,0.3),plot.margin = unit(c(0,0,0,0),"lines"))
theme0 <- function(...) theme( legend.position = "none",
                               panel.background = element_blank(),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.margin = unit(0,"null"),
                               axis.ticks = element_blank(),
                               axis.text.x = element_blank(),
                               axis.text.y = element_blank(),
                               axis.title.x = element_blank(),
                               axis.title.y = element_blank(),
                               axis.ticks.length = unit(0,"null"),
                               axis.ticks.margin = unit(0,"null"),
                               panel.border=element_rect(color=NA),...)
#top, right, bottom, and left margins
plot_top <- ggplot(df,aes(x=GFP,colour=as.factor(concentration))) + 
  geom_density(alpha=0.3) + 
  scale_colour_grey()+
  #scale_colour_manual(values=cbPalette)+
  #scale_colour_manual(values = brewer.pal(10,"Spectral"))+
  #scale_color_brewer(palette=rev("Spectral"))+
  scale_x_log10(name   = "GFP fluorescence, A.U.",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1,1e6)) +theme_bw() +theme0(plot.margin = unit(c(1,0,-0.5,1.55),"lines")) 
              
plot_right <- ggplot(df,aes(x=mCherry,colour=as.factor(concentration))) + 
  geom_density(alpha=0.3) + 
  coord_flip()  +
  scale_colour_grey()+
  #scale_colour_manual(values=cbPalette)+
  #scale_colour_manual(values = brewer.pal(10,"Spectral"))+
  #scale_color_brewer(palette=rev("Spectral"))+
  scale_x_log10(name   = "GFP fluorescence, A.U.",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1,1e6)) +theme_bw() + theme0(plot.margin = unit(c(0,0,1.3,-0.5),"lines"))
            
p_m <- grid.arrange(plot_top, empty, dens2, plot_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
g <- arrangeGrob(plot_top, empty, dens2, plot_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4),main=textGrob(paste('Toggle switch fluorescence densities, with varying aTc concentration, repeat1',sep=""), vjust =1, gp = gpar(fontsize=14, fontface="bold", fontsize=14))) 
ggsave(file="density2d_marginals_all_concentrations_atc_r2_gr.pdf", g,width=12, height=10, dpi=300)
