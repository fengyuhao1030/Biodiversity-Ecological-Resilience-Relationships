rm(list = ls())
library(rstudioapi)
library(openxlsx)
library(ggplot2)
currentPath <- getSourceEditorContext()$path
charLocations <- gregexpr('/',currentPath)[[1]]
currentPath <- substring(currentPath,1,charLocations[length(charLocations)]-1)
setwd(currentPath)

##==== Function ====##
theme_custom <- function(){
  myTheme <- theme(panel.background = element_blank(),
                   panel.grid = element_blank(),
                   legend.position = 'none',
                   plot.margin = margin(3,3,3,3),
                   plot.background = element_blank(),
                   axis.ticks = element_blank(),
                   axis.line.x.bottom = element_line(linewidth = 0.5,arrow = arrow(length = unit(0.04,'inches'),ends = 'last',type = 'closed')),
                   axis.line.y.left = element_line(linewidth = 0.5,arrow = arrow(length = unit(0.04,'inches'),ends = 'last',type = 'closed')),
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.text.x = element_blank())
  return(myTheme)
}
##==== Function ====##

tempX <- seq(-1,1,length.out = 100)
tempY1 <- tempX
tempY2 <- -tempX
drawMatrix1 <- data.frame(X = tempX,Y = tempY1,Type = rep(1,100))
drawMatrix2 <- data.frame(X = tempX,Y = tempY2,Type = rep(2,100))
drawMatrix <- rbind(drawMatrix1,drawMatrix2)

Fig <- ggplot()+
  geom_line(data = drawMatrix,mapping = aes(x = X,y = Y,group = factor(Type),color = factor(Type)))+
  scale_color_manual(values = c('#5469af','#bd494b'))+
  scale_x_continuous(limits = c(-1.1,1.1),expand = c(0,0))+
  scale_y_continuous(limits = c(-1.5,1.5),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_h-1.pdf'
pdf(file <- outputFileName,width = 0.85,height = 0.73)
print(Fig)
dev.off()
outputFileName <- 'Fig_h-2.pdf'
pdf(file <- outputFileName,width = 0.85,height = 0.73)
print(Fig)
dev.off()