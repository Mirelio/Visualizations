library(ggplot2)
library(gridExtra)
library(XML)
library(plyr)
library(grid)
library(scales)


#no_col <- 100
names <- c('cell','geA','repA','rep_r','dim','dim_r','deg','aut_1' ,'aut_2' ,'aut_3' ,'deg_dim','geB','repB')
#res = read.table('res.txt', header=TRUE, sep = '\t')
#p <- ggplot(res,aes(x=value, fill=stability)) + geom_density(alpha=0.25)+
#  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = comma)+
#  facet_wrap( ~ parameter, ncol=2, scales="free")+
#  theme_bw()
#ggsave('param_diffrences_log.pdf',p)
no_col=20000
res_bi = read.table('bistable_res.txt', header=FALSE, fill=TRUE, sep = '\t',col.names=1:no_col)
res_bi <- as.data.frame(t(res_bi))
colnames(res_bi) <- names
res_tri = read.table('tristable_res.txt', header=FALSE, fill=TRUE, sep = '\t',col.names=1:no_col)
res_tri <- as.data.frame(t(res_tri))
colnames(res_tri) <- names
#no_col=200
#res_bth = read.table('both_res2.txt', header=FALSE, fill=TRUE, sep = '\t',col.names=1:no_col)
#res_bth <- as.data.frame(t(res_bth))
#colnames(res_bth) <- names


#ggplot(res_bi,aes(x=nxy))+ geom_density(fill="red",alpha=0.25)+
#  geom_density(data=res_tri,aes(x=nxy),fill='blue',alpha=0.25)+
#  geom_density(data=res_bth,aes(x=nxy),fill='green',alpha=0.25)+
#  theme_bw()
#ggplot(res_bi,aes(x=kx, y=ky)) +geom_density2d(fill="red",alpha=0.5)+
#  geom_density2d(data=res_tri,aes(x=kx, y=ky),fill='blue',alpha=0.5)+
#  geom_density2d(data=res_bth,aes(x=kx, y=ky),fill='green',alpha=0.5)+
#  theme_bw()
names <- c('cell','geA','repA','rep_r','dim','dim_r','deg','aut_1' ,'aut_2' ,'aut_3' ,'deg_dim','geB','repB')

numb_params = length(names)+1
a=c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
b=c(1, 10, 10, 10, 15, 10, 10, 10, 10, 10, 1, 10, 10)
pltList <- list()
k=0
# pltList[[1]] <- ggplot(res_bi,aes(x=gx))+ geom_density(fill="yellow",alpha=0.25)+
#   geom_density(data=res_tri,aes(x=gx),fill='purple',alpha=0.25)+
#   #geom_density(data=res_bth,aes(x=gx),fill='green',alpha=0.25)+
#   #scale_x_log10(limits = c(3,45),labels = comma, breaks=c(3,5,10,20,30,45))+
#   theme_bw()+
#   ggtitle('gx')+
#   ylab("Density")+
#   xlab("Value")+
#   theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))
# 
# pltList[[2]] <- ggplot(res_bi,aes(x=gy))+ geom_density(fill="yellow",alpha=0.25)+
#   geom_density(data=res_tri,aes(x=gy),fill='purple',alpha=0.25)+
#   #geom_density(data=res_bth,aes(x=gy),fill='green',alpha=0.25)+
#   #scale_x_log10(limits = c(3,45),labels = comma, breaks=c(3,5,10,20,30,45))+
#   theme_bw()+
#   ggtitle('gy')+
#   ylab("Density")+
#   xlab("Value")+
#   theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))
for(i in 1:13){
    k=k+1
        pltList[[k]] <-ggplot(res_bi, aes_string(x=names[i])) + geom_density(fill="yellow", alpha=0.35)+
        geom_density(data=res_tri,aes_string(x=names[i]),fill='purple',alpha=0.25)+
        #geom_density(data=res_bth,aes_string(x=names[i]),fill='green',alpha=0.25)+
        xlim(a[i],b[i])+
        ggtitle(names[i])+
        ylab("Density")+
        xlab("Value")+
        theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))
        # ylim(0,5)
        #scale_x_log10(limits = (a[i],b[i]),labels = comma)+
        theme_bw()
}
pdf('res_all2yp.pdf')
do.call("grid.arrange", pltList[2:13])
dev.off()











