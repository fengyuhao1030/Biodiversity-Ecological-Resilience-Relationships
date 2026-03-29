rm(list = ls())
library(rstudioapi)
library(deSolve)
library(ggplot2)
library(R2OpenBUGS)
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
                   axis.ticks = element_line(linewidth = 0.4),
                   axis.ticks.length = unit(-0.25,'lines'),
                   axis.line.x.bottom = element_line(linewidth = 0.7,arrow = arrow(length = unit(0.05,'inches'),ends = 'last',type = 'closed')),
                   axis.line.y.left = element_line(linewidth = 0.7,arrow = arrow(length = unit(0.05,'inches'),ends = 'last',type = 'closed')),
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 8,margin = margin(0,4,0,0),color = '#000000'),
                   axis.text.x = element_text(size = 8,margin = margin(4,0,0,0),color = '#000000'))
  return(myTheme)
}
GenArrow <- function(centerPoint,angle,size){
  # Point 1
  deltaX <- sin(angle/180*pi)*size*0.264
  deltaY <- cos(angle/180*pi)*size
  point1X <- centerPoint[1] + deltaX
  point1Y <- centerPoint[2] + deltaY
  # Point 2
  deltaX <- sin((angle-120)/180*pi)*size*0.264
  deltaY <- cos((angle-120)/180*pi)*size
  point2X <- centerPoint[1] + deltaX
  point2Y <- centerPoint[2] + deltaY
  # Point 3
  deltaX <- sin((angle+120)/180*pi)*size*0.264
  deltaY <- cos((angle+120)/180*pi)*size
  point3X <- centerPoint[1] + deltaX
  point3Y <- centerPoint[2] + deltaY
  # Arrow
  arrowPolygon <- data.frame(X = c(point1X,point2X,point3X,point1X),Y = c(point1Y,point2Y,point3Y,point1Y))
  return(arrowPolygon)
}
##==== Function ====##

dataMatrix1 <- read.csv('Collapse.csv')
dataMatrix1$Type <- 1
tippingpoint1 <- dataMatrix1[nrow(dataMatrix1),]
dataMatrix2 <- read.csv('Recovery.csv')
dataMatrix2$Type <- 2
tippingpoint2 <- dataMatrix2[1,]
dataMatrix <- rbind(dataMatrix1,dataMatrix2)
tippingpoint <- rbind(tippingpoint1,tippingpoint2)
# Line
lineMatrix1 <- data.frame(X = c(as.numeric(tippingpoint$Env[1]),as.numeric(tippingpoint$Env[1])),Y = c(as.numeric(tippingpoint$State[1]),as.numeric(tippingpoint$State[1])-0.2))
lineMatrix2 <- data.frame(X = c(as.numeric(tippingpoint$Env[2]),as.numeric(tippingpoint$Env[2])),Y = c(as.numeric(tippingpoint$State[2]),as.numeric(tippingpoint$State[2])+0.2))
# Arrow
arrow1 <- GenArrow(c(dataMatrix1$Env[80],dataMatrix1$State[80]),120,0.044)
arrow2 <- GenArrow(as.numeric(lineMatrix1[2,]),180,0.044)
arrow3 <- GenArrow(as.numeric(lineMatrix2[2,]),0,0.044)
arrow4 <- GenArrow(c(dataMatrix2$Env[220],dataMatrix2$State[220]),272,0.044)

##==== Draw ====##
Fig <- ggplot()+
  geom_line(data = dataMatrix1,mapping = aes(x = Env,y = State,group = factor(Type),color = Distance),linewidth = 1.7)+
  geom_line(data = dataMatrix2,mapping = aes(x = Env,y = State,group = factor(Type),color = Distance),linewidth = 1.7)+
  geom_line(data = dataMatrix1,mapping = aes(x = Env,y = State,group = factor(Type)),color = '#4258b2',linewidth = 0.7)+
  geom_line(data = lineMatrix1,mapping = aes(x = X,y = Y),color = '#4258b2',linewidth = 0.7)+
  geom_line(data = dataMatrix2,mapping = aes(x = Env,y = State,group = factor(Type)),color = '#428a33',linewidth = 0.7)+
  geom_line(data = lineMatrix2,mapping = aes(x = X,y = Y),color = '#428a33',linewidth = 0.7)+
  geom_point(data = tippingpoint,mapping = aes(x = Env,y = State,group = factor(Type),fill = factor(Type)),shape = 21,size = 2,color = '#000000',stroke = 0.4)+
  geom_polygon(data = arrow1,mapping = aes(x = X,y = Y),fill = '#4258b2',color = '#000000',linewidth = 0.25)+
  geom_polygon(data = arrow2,mapping = aes(x = X,y = Y),fill = '#4258b2',color = '#000000',linewidth = 0.25)+
  geom_polygon(data = arrow3,mapping = aes(x = X,y = Y),fill = '#428a33',color = '#000000',linewidth = 0.25)+
  geom_polygon(data = arrow4,mapping = aes(x = X,y = Y),fill = '#428a33',color = '#000000',linewidth = 0.25)+
  scale_x_continuous(limits = c(-0.024,0.384),breaks = c(0,0.36),labels = c('0','Max'),expand = c(0,0))+
  scale_y_continuous(limits = c(-0.25,1.2),breaks = c(0,1),labels = c('0','K'),expand = c(0,0))+
  scale_color_gradient(low = '#e61c17',high = '#fbf300')+
  scale_fill_manual(values = c('#4258b2','#428a33'))+
  theme_custom()
outputFileName <- 'Fig_1e.pdf'
pdf(file <- outputFileName,width = 1.71,height = 1.5)
print(Fig)
dev.off()
##==== Draw ====##