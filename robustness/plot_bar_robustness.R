library(ggplot2)

#pos2 = c(0.573230769231, 0.574384615385, 0.576615384615)
#std = c(0.3346, 0.3383, 0.3389)

std= c(0.103, 0.129, 0.117)
pos2 = c(0.649, 0.61, 0.635)
gard_stc = c(0.795, 0.81, 0.817)
gard_det = c(0.001, 0.002, 0.002)

data = data.frame(
  switches = c("simple", "simple", "simple", "feedback", "feedback","feedback"),
  levels = c(0.3346, 0.3383, 0.3389, 0.573230769231, 0.574384615385, 0.576615384615)
)

data_means_gard = data.frame(
  switches = c('deterministic','stochastic'),
  means = c(0.0017, 0.807),
  stdevs = c(0.0006,0.011))

data_means = data.frame(
  switches = c('simple','feedback'),
  means = c(0.116, 0.631),
  stdevs = c(0.013,0.0198))

pl <- ggplot(data_means, aes(x=switches, y=means, fill=switches)) +
  scale_fill_brewer(palette = 'Accent')+
  geom_bar(width=.5, position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=means-stdevs, ymax=means+stdevs),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Model") +
  ylab("Robustness") +
  geom_text(aes(y=means, ymax=means, label=means), position= position_dodge(width=0.9), vjust=-1, color="black")+
  scale_colour_brewer(palette="Set1")+
  theme_bw()+
  theme(axis.line=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        #panel.background=element_blank(),
        #panel.border=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

ggsave(pl, file="robustness_comparison.pdf")
