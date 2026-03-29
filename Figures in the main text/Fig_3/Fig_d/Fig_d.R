rm(list = ls())
library(rstudioapi)
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
                   axis.line.x.bottom = element_line(linewidth = 0.7,arrow = arrow(length = unit(0.04,'inches'),ends = 'last',type = 'closed')),
                   axis.line.y.left = element_line(linewidth = 0.7,arrow = arrow(length = unit(0.04,'inches'),ends = 'last',type = 'closed')),
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.text.x = element_blank())
  return(myTheme)
}
##==== Function ====##

set.seed(1000)
errors <- rnorm(n = 50,mean = 0,sd = 0.05)
tempX <- seq(0,1,length.out = 50)
tempY <- tempX*0.5 + errors
drawMatrix1 <- data.frame(X = tempX,Y = tempY)
Fig <- ggplot()+
  geom_line(data = drawMatrix1,mapping = aes(x = X,y = Y),color = '#4572c4',linewidth = 0.5)+
  scale_x_continuous(limits = c(-0.05,1.05),expand = c(0,0))+
  scale_y_continuous(limits = c(-0.2,0.65),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_d.pdf'
pdf(file <- outputFileName,width = 1,height = 0.8)
print(Fig)
dev.off()