# library(dplyr)
# library(ggplot)
# library(ggrepel)

volcano = volcano %>% mutate(Significance = if_else(adj.P.Val < 0.05 & abs(logFC) > 1,
                                          "adj.P<0.05 & FC>1",
                                          "Not Significant")) 
volcano %>% 
ggplot() +
    geom_point(aes(x=logFC, y=-log10(adj.P.Val), color=Significance), alpha=0.8) +
    geom_vline(xintercept = c(-1,1), linetype=2) + 
    geom_hline(yintercept = -log10(0.05), linetype=2)+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    scale_color_brewer(type = qualitative, palette="Set2")+
    geom_text(x=-2.5, y =2.5,
              label=paste(sum(nrow(volcano[volcano$logFC < -1 & volcano$Significance == "adj.P<0.05 & FC>1", ])), "down significant"),
              color = "blue")+
    geom_text(x=2, y =2.5,
              label=paste(sum(nrow(volcano[volcano$logFC > 1 & volcano$Significance == "adj.P<0.05 & FC>1", ])), "up significant"),
              color = "blue") + 
  geom_text_repel(data=head(volcano, 20), aes(x = logFC, y = -log10(adj.P.Val), label=symbol))
