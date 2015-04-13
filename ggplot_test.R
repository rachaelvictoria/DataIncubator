library(ggplot2)
library(reshape)


x = c("Band 1", "Band 2", "Band 3")
y1 = c("1","2","3")
y2 = c("2","3","4")

to_plot <- data.frame(x=x,y1=y1,y2=y2)
melted<-melt(to_plot, id="x")

print(ggplot(melted,aes(x=x,y=value,fill=variable)) + geom_bar(stat="identity", alpha=.3))