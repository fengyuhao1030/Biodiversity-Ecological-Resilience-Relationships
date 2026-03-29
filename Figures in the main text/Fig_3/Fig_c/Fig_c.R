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
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.text.x = element_blank())
  return(myTheme)
}
##==== Function ====##

## Data
set.seed(1)
vec <- rnorm(100,mean = 0,sd = 1)
dataMatrix <- matrix(vec,nrow = 10,ncol = 10,byrow = TRUE)
drawMatrix_1 <- data.frame()
for(i in seq(1,nrow(dataMatrix))){
  tempX <- seq(1,10)
  tempY <- rep(i,10)
  tempValue <- dataMatrix[i,]
  tempMatrix <- data.frame(X = tempX,Y = tempY,Value = tempValue)
  drawMatrix_1 <- rbind(drawMatrix_1,tempMatrix)
}
set.seed(2)
vec <- rnorm(100,mean = 0,sd = 1)
dataMatrix <- matrix(vec,nrow = 10,ncol = 10,byrow = TRUE)
drawMatrix_2 <- data.frame()
for(i in seq(1,nrow(dataMatrix))){
  tempX <- seq(1,10)
  tempY <- rep(i,10)
  tempValue <- dataMatrix[i,]
  tempMatrix <- data.frame(X = tempX,Y = tempY,Value = tempValue)
  drawMatrix_2 <- rbind(drawMatrix_2,tempMatrix)
}
set.seed(3)
vec <- rnorm(100,mean = 0,sd = 1)
dataMatrix <- matrix(vec,nrow = 10,ncol = 10,byrow = TRUE)
drawMatrix_3 <- data.frame()
for(i in seq(1,nrow(dataMatrix))){
  tempX <- seq(1,10)
  tempY <- rep(i,10)
  tempValue <- dataMatrix[i,]
  tempMatrix <- data.frame(X = tempX,Y = tempY,Value = tempValue)
  drawMatrix_3 <- rbind(drawMatrix_3,tempMatrix)
}
set.seed(4)
vec <- rnorm(100,mean = 0,sd = 1)
dataMatrix <- matrix(vec,nrow = 10,ncol = 10,byrow = TRUE)
drawMatrix_4 <- data.frame()
for(i in seq(1,nrow(dataMatrix))){
  tempX <- seq(1,10)
  tempY <- rep(i,10)
  tempValue <- dataMatrix[i,]
  tempMatrix <- data.frame(X = tempX,Y = tempY,Value = tempValue)
  drawMatrix_4 <- rbind(drawMatrix_4,tempMatrix)
}

## Lines
lineMatrix <- data.frame()
for(i in seq(1,9)){
  tempX <- i + 0.5
  tempXEnd <- i + 0.5
  tempY <- 0.5
  tempYEnd <- 10.5
  tempMatrix <- data.frame(X = tempX,XEnd = tempXEnd,Y = tempY,YEnd = tempYEnd)
  lineMatrix <- rbind(lineMatrix,tempMatrix)
}
for(i in seq(1,9)){
  tempX <- 0.5
  tempXEnd <- 10.5
  tempY <- i + 0.5
  tempYEnd <- i + 0.5
  tempMatrix <- data.frame(X = tempX,XEnd = tempXEnd,Y = tempY,YEnd = tempYEnd)
  lineMatrix <- rbind(lineMatrix,tempMatrix)
}
extentMatrix <- data.frame()
for(i in c(0,10)){
  tempX <- i + 0.5
  tempXEnd <- i + 0.5
  tempY <- 0.5
  tempYEnd <- 10.5
  tempMatrix <- data.frame(X = tempX,XEnd = tempXEnd,Y = tempY,YEnd = tempYEnd)
  extentMatrix <- rbind(extentMatrix,tempMatrix)
}
for(i in c(0,10)){
  tempX <- 0.5
  tempXEnd <- 10.5
  tempY <- i + 0.5
  tempYEnd <- i + 0.5
  tempMatrix <- data.frame(X = tempX,XEnd = tempXEnd,Y = tempY,YEnd = tempYEnd)
  extentMatrix <- rbind(extentMatrix,tempMatrix)
}

## Draw
Fig <- ggplot()+
  geom_raster(data = drawMatrix_1,mapping = aes(x = X,y = Y,fill = Value))+
  geom_segment(data = lineMatrix,mapping = aes(x = X,xend = XEnd,y = Y,yend = YEnd),color = '#999999',linewidth = 0.15)+
  geom_segment(data = extentMatrix,mapping = aes(x = X,xend = XEnd,y = Y,yend = YEnd),color = '#000000',linewidth = 0.3)+
  scale_fill_gradient2(low = '#bd494b',mid = '#fffbca',high = '#5469af')+
  theme_custom()
outputFileName <- 'Fig_c-1.pdf'
pdf(file <- outputFileName,width = 0.67,height = 0.67)
print(Fig)
dev.off()
Fig <- ggplot()+
  geom_raster(data = drawMatrix_2,mapping = aes(x = X,y = Y,fill = Value))+
  geom_segment(data = lineMatrix,mapping = aes(x = X,xend = XEnd,y = Y,yend = YEnd),color = '#999999',linewidth = 0.15)+
  geom_segment(data = extentMatrix,mapping = aes(x = X,xend = XEnd,y = Y,yend = YEnd),color = '#000000',linewidth = 0.3)+
  scale_fill_gradient2(low = '#bd494b',mid = '#fffbca',high = '#5469af')+
  theme_custom()
outputFileName <- 'Fig_c-2.pdf'
pdf(file <- outputFileName,width = 0.67,height = 0.67)
print(Fig)
dev.off()
Fig <- ggplot()+
  geom_raster(data = drawMatrix_3,mapping = aes(x = X,y = Y,fill = Value))+
  geom_segment(data = lineMatrix,mapping = aes(x = X,xend = XEnd,y = Y,yend = YEnd),color = '#999999',linewidth = 0.15)+
  geom_segment(data = extentMatrix,mapping = aes(x = X,xend = XEnd,y = Y,yend = YEnd),color = '#000000',linewidth = 0.3)+
  scale_fill_gradient2(low = '#bd494b',mid = '#fffbca',high = '#5469af')+
  theme_custom()
outputFileName <- 'Fig_c-3.pdf'
pdf(file <- outputFileName,width = 0.67,height = 0.67)
print(Fig)
dev.off()
Fig <- ggplot()+
  geom_raster(data = drawMatrix_4,mapping = aes(x = X,y = Y,fill = Value))+
  geom_segment(data = lineMatrix,mapping = aes(x = X,xend = XEnd,y = Y,yend = YEnd),color = '#999999',linewidth = 0.15)+
  geom_segment(data = extentMatrix,mapping = aes(x = X,xend = XEnd,y = Y,yend = YEnd),color = '#000000',linewidth = 0.3)+
  scale_fill_gradient2(low = '#bd494b',mid = '#fffbca',high = '#5469af')+
  theme_custom()
outputFileName <- 'Fig_c-4.pdf'
pdf(file <- outputFileName,width = 0.67,height = 0.67)
print(Fig)
dev.off()