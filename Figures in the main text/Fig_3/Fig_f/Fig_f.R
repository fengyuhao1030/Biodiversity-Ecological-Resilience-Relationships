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

##==== High stable state ====##
env <- seq(-1,1,length.out = 100)
ecoState <- env
distance <- env
drawMatrix <- data.frame(Env = env,EcoState = ecoState,Distance = distance)
Fig <- ggplot()+
  geom_line(data = drawMatrix,mapping = aes(x = Env,y = EcoState),linewidth = 0.7)+
  scale_x_continuous(limits = c(-1.3,1.3),expand = c(0,0))+
  scale_y_continuous(limits = c(-1.3,1.3),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_f_High_Stable_State_1.pdf'
pdf(file <- outputFileName,width = 0.8,height = 0.7)
print(Fig)
dev.off()
Fig <- ggplot()+
  geom_line(data = drawMatrix,mapping = aes(x = Env,y = Distance),linewidth = 0.7)+
  scale_x_continuous(limits = c(-1.3,1.3),expand = c(0,0))+
  scale_y_continuous(limits = c(-1.3,1.3),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_f_High_Stable_State_2.pdf'
pdf(file <- outputFileName,width = 0.8,height = 0.7)
print(Fig)
dev.off()
Fig <- ggplot()+
  geom_line(data = drawMatrix,mapping = aes(x = EcoState,y = Distance),linewidth = 0.7)+
  scale_x_continuous(limits = c(-1.3,1.3),expand = c(0,0))+
  scale_y_continuous(limits = c(-1.3,1.3),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_f_High_Stable_State_3.pdf'
pdf(file <- outputFileName,width = 0.8,height = 0.7)
print(Fig)
dev.off()
##==== High stable state ====##

##==== Low stable state ====##
env <- seq(-1,1,length.out = 100)
ecoState <- env
distance <- -env
drawMatrix <- data.frame(Env = env,EcoState = ecoState,Distance = distance)
Fig <- ggplot()+
  geom_line(data = drawMatrix,mapping = aes(x = Env,y = EcoState),linewidth = 0.7)+
  scale_x_continuous(limits = c(-1.3,1.3),expand = c(0,0))+
  scale_y_continuous(limits = c(-1.3,1.3),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_f_Low_Stable_State_1.pdf'
pdf(file <- outputFileName,width = 0.8,height = 0.7)
print(Fig)
dev.off()
Fig <- ggplot()+
  geom_line(data = drawMatrix,mapping = aes(x = Env,y = Distance),linewidth = 0.7)+
  scale_x_continuous(limits = c(-1.3,1.3),expand = c(0,0))+
  scale_y_continuous(limits = c(-1.3,1.3),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_f_Low_Stable_State_2.pdf'
pdf(file <- outputFileName,width = 0.8,height = 0.7)
print(Fig)
dev.off()
Fig <- ggplot()+
  geom_line(data = drawMatrix,mapping = aes(x = EcoState,y = Distance),linewidth = 0.7)+
  scale_x_continuous(limits = c(-1.3,1.3),expand = c(0,0))+
  scale_y_continuous(limits = c(-1.3,1.3),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_f_Low_Stable_State_3.pdf'
pdf(file <- outputFileName,width = 0.8,height = 0.7)
print(Fig)
dev.off()
##==== Low stable state ====##

##==== Bistable states ====##
dataMatrix <- read.xlsx('Data.xlsx',sheet = 1)
selectIDs <- which(dataMatrix$Type == 1)
drawMatrix_1 <- dataMatrix[selectIDs,]
selectIDs <- which(dataMatrix$Type == 2)
drawMatrix_2 <- dataMatrix[selectIDs,]
selectIDs <- which(dataMatrix$Type == 3)
drawMatrix_3 <- dataMatrix[selectIDs,]
Fig <- ggplot()+
  geom_line(data = drawMatrix_1,mapping = aes(x = Env,y = EcoState),color = '#58b038',linewidth = 0.7)+
  geom_line(data = drawMatrix_3,mapping = aes(x = Env,y = EcoState),color = '#f2a319',linewidth = 0.7)+
  scale_x_continuous(limits = c(-4.5,4.5),expand = c(0,0))+
  scale_y_continuous(limits = c(-3.8,3.8),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_f_Bistable_State_1.pdf'
pdf(file <- outputFileName,width = 0.8,height = 0.7)
print(Fig)
dev.off()
env <- seq(-1,1,length.out = 100)
ecoState <- env
distance <- env
drawMatrix <- data.frame(Env = env,EcoState = ecoState,Distance = distance)
Fig <- ggplot()+
  geom_line(data = drawMatrix,mapping = aes(x = Env,y = Distance),color = '#58b038',linewidth = 0.7)+
  scale_x_continuous(limits = c(-1.3,1.3),expand = c(0,0))+
  scale_y_continuous(limits = c(-1.3,1.3),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_f_Bistable_State_2.pdf'
pdf(file <- outputFileName,width = 0.8,height = 0.7)
print(Fig)
dev.off()
Fig <- ggplot()+
  geom_line(data = drawMatrix,mapping = aes(x = EcoState,y = Distance),color = '#58b038',linewidth = 0.7)+
  scale_x_continuous(limits = c(-1.3,1.3),expand = c(0,0))+
  scale_y_continuous(limits = c(-1.3,1.3),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_f_Bistable_State_3.pdf'
pdf(file <- outputFileName,width = 0.8,height = 0.7)
print(Fig)
dev.off()
env <- seq(-1,1,length.out = 100)
ecoState <- env
distance <- -env
drawMatrix <- data.frame(Env = env,EcoState = ecoState,Distance = distance)
Fig <- ggplot()+
  geom_line(data = drawMatrix,mapping = aes(x = Env,y = Distance),color = '#f2a319',linewidth = 0.7)+
  scale_x_continuous(limits = c(-1.3,1.3),expand = c(0,0))+
  scale_y_continuous(limits = c(-1.3,1.3),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_f_Bistable_State_4.pdf'
pdf(file <- outputFileName,width = 0.8,height = 0.7)
print(Fig)
dev.off()
Fig <- ggplot()+
  geom_line(data = drawMatrix,mapping = aes(x = EcoState,y = Distance),color = '#f2a319',linewidth = 0.7)+
  scale_x_continuous(limits = c(-1.3,1.3),expand = c(0,0))+
  scale_y_continuous(limits = c(-1.3,1.3),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_f_Bistable_State_5.pdf'
pdf(file <- outputFileName,width = 0.8,height = 0.7)
print(Fig)
dev.off()
##==== Bistable states ====##