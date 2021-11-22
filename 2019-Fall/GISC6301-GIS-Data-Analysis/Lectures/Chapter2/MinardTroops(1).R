library(HistData)           # needs to be downloaded
library(ggplot2)
data(Minard.troops); data(Minard.cities)

plot_troops <- ggplot(Minard.troops, aes(long, lat)) +
  geom_path(aes(size = survivors, colour = direction, group = group))

plot_both <- plot_troops + 
  geom_text(aes(label = city), size = 4, data = Minard.cities)

plot_polished <- plot_both + 
  scale_colour_manual(values = c("grey50","red")) +
  xlab(NULL) + 
  ylab(NULL)

# re-scale the plot window to an aspect ratio of ~ 4 x 1
windows(width=12, height=3)
plot_polished
