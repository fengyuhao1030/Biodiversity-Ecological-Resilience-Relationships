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
  deltaX <- sin(angle/180*pi)*size*0.155
  deltaY <- cos(angle/180*pi)*size
  point1X <- centerPoint[1] + deltaX
  point1Y <- centerPoint[2] + deltaY
  # Point 2
  deltaX <- sin((angle-120)/180*pi)*size*0.155
  deltaY <- cos((angle-120)/180*pi)*size
  point2X <- centerPoint[1] + deltaX
  point2Y <- centerPoint[2] + deltaY
  # Point 3
  deltaX <- sin((angle+120)/180*pi)*size*0.155
  deltaY <- cos((angle+120)/180*pi)*size
  point3X <- centerPoint[1] + deltaX
  point3Y <- centerPoint[2] + deltaY
  # Arrow
  arrowPolygon <- data.frame(X = c(point1X,point2X,point3X,point1X),Y = c(point1Y,point2Y,point3Y,point1Y))
  return(arrowPolygon)
}
##==== Function ====##

dataMatrix1 <- read.csv('OneSpecies_Collapse.csv')
dataMatrix2 <- read.csv('OneSpecies_Recovery.csv')
dataMatrix3 <- read.csv('TwoSpecies_Collapse.csv')
dataMatrix4 <- read.csv('TwoSpecies_Recovery.csv')
dataMatrix5 <- read.csv('ThreeSpecies_Collapse.csv')
dataMatrix6 <- read.csv('ThreeSpecies_Recovery.csv')
# Tippingpoint
tippingpoint1 <- dataMatrix1[nrow(dataMatrix1),]
tippingpoint2 <- dataMatrix2[nrow(dataMatrix2),]
tippingpoint3 <- dataMatrix3[nrow(dataMatrix3),]
tippingpoint4 <- dataMatrix4[nrow(dataMatrix4),]
tippingpoint5 <- dataMatrix5[nrow(dataMatrix5),]
tippingpoint6 <- dataMatrix6[nrow(dataMatrix6),]
# Line
lineMatrix1 <- data.frame(X = c(as.numeric(tippingpoint1$Env),as.numeric(tippingpoint1$Env)),Y = c(as.numeric(tippingpoint1$State),as.numeric(tippingpoint1$State)-0.35))
lineMatrix2 <- data.frame(X = c(as.numeric(tippingpoint2$Env),as.numeric(tippingpoint2$Env)),Y = c(as.numeric(tippingpoint2$State),as.numeric(tippingpoint2$State)+0.35))
lineMatrix3 <- data.frame(X = c(as.numeric(tippingpoint3$Env),as.numeric(tippingpoint3$Env)),Y = c(as.numeric(tippingpoint3$State),as.numeric(tippingpoint3$State)-0.35))
lineMatrix4 <- data.frame(X = c(as.numeric(tippingpoint4$Env),as.numeric(tippingpoint4$Env)),Y = c(as.numeric(tippingpoint4$State),as.numeric(tippingpoint4$State)+0.35))
lineMatrix5 <- data.frame(X = c(as.numeric(tippingpoint5$Env),as.numeric(tippingpoint5$Env)),Y = c(as.numeric(tippingpoint5$State),as.numeric(tippingpoint5$State)-0.35))
lineMatrix6 <- data.frame(X = c(as.numeric(tippingpoint6$Env),as.numeric(tippingpoint6$Env)),Y = c(as.numeric(tippingpoint6$State),as.numeric(tippingpoint6$State)+0.35))
# Arrow
arrow1 <- GenArrow(c(dataMatrix1$Env[80],dataMatrix1$State[80]),110,0.077)
arrow2 <- GenArrow(c(lineMatrix1$X[2],lineMatrix1$Y[2]),180,0.077)

arrow3 <- GenArrow(c(dataMatrix2$Env[100],dataMatrix2$State[100]),272,0.077)
arrow4 <- GenArrow(c(lineMatrix2$X[2],lineMatrix2$Y[2]),0,0.077)

arrow5 <- GenArrow(c(dataMatrix3$Env[120],dataMatrix3$State[120]),115,0.077)
arrow6 <- GenArrow(c(lineMatrix3$X[2],lineMatrix3$Y[2]),180,0.077)

arrow7 <- GenArrow(c(dataMatrix4$Env[190],dataMatrix4$State[190]),272,0.077)
arrow8 <- GenArrow(c(lineMatrix4$X[2],lineMatrix4$Y[2]),0,0.077)

arrow9 <- GenArrow(c(dataMatrix5$Env[150],dataMatrix5$State[150]),115,0.077)
arrow10 <- GenArrow(c(lineMatrix5$X[2],lineMatrix5$Y[2]),180,0.077)

arrow11 <- GenArrow(c(dataMatrix6$Env[300],dataMatrix6$State[300]),272,0.077)
arrow12 <- GenArrow(c(lineMatrix6$X[2],lineMatrix6$Y[2]),0,0.08)

Fig <- ggplot()+
  geom_line(data = dataMatrix1,mapping = aes(x = Env,y = State,color = Distance),linewidth = 1.7)+
  geom_line(data = dataMatrix1,mapping = aes(x = Env,y = State),color = '#4258b2',linewidth = 0.7)+
  geom_line(data = lineMatrix1,mapping = aes(x = X,y = Y),color = '#4258b2',linewidth = 0.7)+
  geom_point(data = tippingpoint1,mapping = aes(x = Env,y = State),fill = '#4258b2',shape = 21,size = 2,color = '#000000',stroke = 0.4)+
  geom_polygon(data = arrow1,mapping = aes(x = X,y = Y),fill = '#4258b2',color = '#000000',linewidth = 0.25)+
  geom_polygon(data = arrow2,mapping = aes(x = X,y = Y),fill = '#4258b2',color = '#000000',linewidth = 0.25)+
  
  geom_line(data = dataMatrix2,mapping = aes(x = Env,y = State,color = Distance),linewidth = 1.7)+
  geom_line(data = dataMatrix2,mapping = aes(x = Env,y = State),color = '#428a33',linewidth = 0.7)+
  geom_line(data = lineMatrix2,mapping = aes(x = X,y = Y),color = '#428a33',linewidth = 0.7)+
  geom_point(data = tippingpoint2,mapping = aes(x = Env,y = State),fill = '#428a33',shape = 21,size = 2,color = '#000000',stroke = 0.4)+
  geom_polygon(data = arrow3,mapping = aes(x = X,y = Y),fill = '#428a33',color = '#000000',linewidth = 0.25)+
  geom_polygon(data = arrow4,mapping = aes(x = X,y = Y),fill = '#428a33',color = '#000000',linewidth = 0.25)+
  
  geom_line(data = dataMatrix3,mapping = aes(x = Env,y = State,color = Distance),linewidth = 1.7)+
  geom_line(data = dataMatrix3,mapping = aes(x = Env,y = State),linetype = '22222222',color = '#4258b2',linewidth = 0.7)+
  geom_line(data = lineMatrix3,mapping = aes(x = X,y = Y),linetype = '22222222',color = '#4258b2',linewidth = 0.7)+
  geom_point(data = tippingpoint3,mapping = aes(x = Env,y = State),fill = '#4258b2',shape = 21,size = 2,color = '#000000',stroke = 0.4)+
  geom_polygon(data = arrow5,mapping = aes(x = X,y = Y),fill = '#4258b2',color = '#000000',linewidth = 0.25)+
  geom_polygon(data = arrow6,mapping = aes(x = X,y = Y),fill = '#4258b2',color = '#000000',linewidth = 0.25)+
  
  geom_line(data = dataMatrix4,mapping = aes(x = Env,y = State,color = Distance),linewidth = 1.7)+
  geom_line(data = dataMatrix4,mapping = aes(x = Env,y = State),linetype = '22222222',color = '#428a33',linewidth = 0.7)+
  geom_line(data = lineMatrix4,mapping = aes(x = X,y = Y),linetype = '22222222',color = '#428a33',linewidth = 0.7)+
  geom_point(data = tippingpoint4,mapping = aes(x = Env,y = State),fill = '#428a33',shape = 21,size = 2,color = '#000000',stroke = 0.4)+
  geom_polygon(data = arrow7,mapping = aes(x = X,y = Y),fill = '#428a33',color = '#000000',linewidth = 0.25)+
  geom_polygon(data = arrow8,mapping = aes(x = X,y = Y),fill = '#428a33',color = '#000000',linewidth = 0.25)+
  
  geom_line(data = dataMatrix5,mapping = aes(x = Env,y = State,color = Distance),linewidth = 1.7)+
  geom_line(data = dataMatrix5,mapping = aes(x = Env,y = State),linetype = '11111111',color = '#4258b2',linewidth = 0.7)+
  geom_line(data = lineMatrix5,mapping = aes(x = X,y = Y),linetype = '11111111',color = '#4258b2',linewidth = 0.7)+
  geom_point(data = tippingpoint5,mapping = aes(x = Env,y = State),fill = '#4258b2',shape = 21,size = 2,color = '#000000',stroke = 0.4)+
  geom_polygon(data = arrow9,mapping = aes(x = X,y = Y),fill = '#4258b2',color = '#000000',linewidth = 0.25)+
  geom_polygon(data = arrow10,mapping = aes(x = X,y = Y),fill = '#4258b2',color = '#000000',linewidth = 0.25)+
  
  geom_line(data = dataMatrix6,mapping = aes(x = Env,y = State,color = Distance),linewidth = 1.7)+
  geom_line(data = dataMatrix6,mapping = aes(x = Env,y = State),linetype = '11111111',color = '#428a33',linewidth = 0.7)+
  geom_line(data = lineMatrix6,mapping = aes(x = X,y = Y),linetype = '11111111',color = '#428a33',linewidth = 0.7)+
  geom_point(data = tippingpoint6,mapping = aes(x = Env,y = State),fill = '#428a33',shape = 21,size = 2,color = '#000000',stroke = 0.4)+
  geom_polygon(data = arrow11,mapping = aes(x = X,y = Y),fill = '#428a33',color = '#000000',linewidth = 0.25)+
  geom_polygon(data = arrow12,mapping = aes(x = X,y = Y),fill = '#428a33',color = '#000000',linewidth = 0.25)+
  
  scale_x_continuous(limits = c(-0.024,0.384),breaks = c(0,0.36),labels = c('0','Max'),expand = c(0,0))+
  scale_y_continuous(limits = c(-0.18,2.2),breaks = seq(0,2,0.5),labels = c('0','0.5','1.0','1.5','2.0'),expand = c(0,0))+
  scale_color_gradient(low = '#e61c17',high = '#fbf300')+
  theme_custom()
outputFileName <- 'Fig_c-2.pdf'
pdf(file <- outputFileName,width = 1.8,height = 1.5)
print(Fig)
dev.off()