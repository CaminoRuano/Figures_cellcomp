library(ggplot2)
library(ggpubr)
library(ggarrange)

setwd("")

## set parameters
my_comparisons = list(c("Control","Diseased"))
my_color = c("#f47b8e", "#c578eb", "#eddf8e","#5ebde5", "#85f37b", '#bcbcbc', '#5b5b5b')
my_breaks = c("CTRL", "chorioamnionitis", "IUGR", "PE", "PE_IUGR", "Control", "Diseased")

# create plots 

p <- ggplot(cell, aes(x = pat, y = STB)) + ggtitle("Syncytiotrophoblast") +
  ylab("Percentage Syncytiotrophoblast")+
  theme(plot.title =element_text(hjust = 0.5, size=25)) + 
  geom_boxplot(outlier.shape = NA, aes(fill=pat))+
  scale_fill_manual(breaks = my_breaks,
                    values=my_color)+
  ylim(0, 0.8)+
  stat_compare_means(pat~STB, data=cell, method="t.test", paired=FALSE, comparisons=my_comparisons, size=8,label="p.signif")+
  geom_jitter(size=2,
              aes(color=pathology),
              position = position_jitter(0.31)) +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  theme(panel.grid = element_line(color="grey", size=0.1, linetype=1), 
        axis.line = element_line(colour = "black"), panel.background = element_rect(fill = "white"))



pt <- ggplot(cell, aes(x = pat, y = TB)) + ggtitle("Trophoblast") +
  ylab("Percentage Trophoblast")+
  theme(plot.title =element_text(hjust = 0.5, size=25)) + 
  geom_boxplot(outlier.shape = NA, aes(fill=pat))+
  scale_fill_manual(breaks = my_breaks,
                    values=my_color)+
  ylim(0, 0.45)+
  stat_compare_means(pat~TB, data=cell, method="t.test", paired=FALSE, comparisons=my_comparisons, size=8,label="p.signif")+
  geom_jitter(size=2,
              aes(color=pathology),
              position = position_jitter(0.31)) +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  theme(panel.grid = element_line(color="grey", size=0.1, linetype=1), 
        axis.line = element_line(colour = "black"), panel.background = element_rect(fill = "white"))



pr <- ggplot(cell, aes(x = pat, y = nRBC)) + ggtitle("Red Blood Cells") +
  ylab("Percentage Red Blood Cells")+
  theme(plot.title =element_text(hjust = 0.5, size=25)) + 
  geom_boxplot(outlier.shape = NA, aes(fill=pat))+
  scale_fill_manual(breaks = c("CTRL", "chorioamnionitis", "IUGR", "PE", "PE_IUGR", "Control", "Diseased"),
                    values=c("#f47b8e", "#c578eb", "#eddf8e","#5ebde5", "#85f37b", '#bcbcbc', '#5b5b5b'))+
  ylim(0, 0.15)+
  stat_compare_means(pat~nRBC, data=cell, method="t.test", paired=FALSE , comparisons=my_comparisons, size=8,label="p.signif")+
  geom_jitter(size=2,
              aes(color=pathology),
              position = position_jitter(0.31)) +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  theme(panel.grid = element_line(color="grey", size=0.1, linetype=1), 
        axis.line = element_line(colour = "black"), panel.background = element_rect(fill = "white"))




pm <- ggplot(cell, aes(x = pat, y = MO)) + ggtitle("Macrophages") +
  ylab("Percentage Macrophages")+
  theme(plot.title =element_text(hjust = 0.5, size=25)) + 
  geom_boxplot(outlier.shape = NA, aes(fill=pat))+
  scale_fill_manual(breaks = c("CTRL", "chorioamnionitis", "IUGR", "PE", "PE_IUGR", "Control", "Diseased"),
                    values=c("#f47b8e", "#c578eb", "#eddf8e","#5ebde5", "#85f37b", '#bcbcbc', '#5b5b5b'))+
  ylim(0, 0.2)+
  stat_compare_means(pat~MO, data=cell, method="t.test", paired=FALSE, comparisons=my_comparisons, size=8,label="p.signif")+
  geom_jitter(size=2,
              aes(color=pathology),
              position = position_jitter(0.31)) +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  theme(panel.grid = element_line(color="grey", size=0.1, linetype=1), 
        axis.line = element_line(colour = "black"), panel.background = element_rect(fill = "white"))



pe<- ggplot(cell, aes(x = pat, y = ENT)) + ggtitle("Endothelial") +
  ylab("Percentage Endothelial")+
  theme(plot.title =element_text(hjust = 0.5, size=25)) + 
  geom_boxplot(outlier.shape = NA, aes(fill=pat))+
  scale_fill_manual(breaks = c("CTRL", "chorioamnionitis", "IUGR", "PE", "PE_IUGR", "Control", "Diseased"),
                    values=c("#f47b8e", "#c578eb", "#eddf8e","#5ebde5", "#85f37b", '#bcbcbc', '#5b5b5b'))+
  ylim(0, 0.25)+
  stat_compare_means(pat~ENT, data=cell, method="t.test", paired=FALSE, comparisons=my_comparisons, size=8,label="p.signif")+
  geom_jitter(size=2,
              aes(color=pathology),
              position = position_jitter(0.31)) +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  theme(panel.grid = element_line(color="grey", size=0.1, linetype=1), 
        axis.line = element_line(colour = "black"), panel.background = element_rect(fill = "white"))


ps<- ggplot(cell, aes(x = pat, y = Stromal)) + ggtitle("Stromal") +
  ylab("Percentage Stromal")+
  theme(plot.title =element_text(hjust = 0.5, size=25)) + 
  geom_boxplot(outlier.shape = NA, aes(fill=pat))+
  scale_fill_manual(breaks = my_breaks,
                    values=cmy_color)+
  ylim(0, 0.25)+
  stat_compare_means(pat~Stromal, data=cell, method="t.test", paired=FALSE, comparisons=my_comparisons, size=8,label="p.signif")+
  geom_jitter(size=2,
              aes(color=pathology),
              position = position_jitter(0.31)) +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  theme(panel.grid = element_line(color="grey", size=0.1, linetype=1), 
        axis.line = element_line(colour = "black"), panel.background = element_rect(fill = "white"))


## merge plots in a single figure

plot =ggarrange(p, pe, pm, pr, ps, pt, ncol=2, nrow=3,common.legend = TRUE, legend="left")

plot

ggsave(plot = plot, filename = "./Cell_composition_Ctrl_vs_Diseased2.png", dpi = 320,  width = 20, height = 20, limitsize=FALSE)





