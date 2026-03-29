rm(list = ls())
library(rstudioapi)
library(terra)
library(ggplot2)
library(see)
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
                   axis.ticks = element_line(linewidth = 0.3),
                   axis.ticks.length = unit(-0.15,'lines'),
                   axis.line.x.bottom = element_line(linewidth = 0.5),
                   axis.line.y.left = element_line(linewidth = 0.5),
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 9,margin = margin(0,4,0,0),color = '#000000'),
                   axis.text.x = element_text(size = 8,margin = margin(4,0,0,0),color = '#000000'))
  return(myTheme)
}
##==== Function ====##

offset <- 0.1
ecoVars <- c('GIMMSKNDVI','PKUKNDVI','LUEGPP','MUSESGPP','NIRVGPP','CASAGIMMS','CASAPKU','MUSESNPP','VODCA')

##==== TEM ====##
disCoeffMatrix <- data.frame()
for(i in seq(1,length(ecoVars))){
  fileName <- paste0('./Data/',ecoVars[i],'_TEM/Low_Stable_State.csv')
  dataMatrix <- read.csv(fileName)
  dataMatrix <- dataMatrix[seq(1,nrow(dataMatrix)-1),]
  selectIDs_1 <- which(dataMatrix$DiversityCoeff > 0)
  selectIDs_2 <- which(dataMatrix$DiversityCoeff < 0)
  if(length(selectIDs_1) > length(selectIDs_2)){
    tempColor <- 1
  }else{
    tempColor <- 2
  }
  tempMatrix <- data.frame(X = length(ecoVars)+1-i+offset,Coeff = as.numeric(dataMatrix$DiversityCoeff),Class = rep(length(ecoVars)+1-i,nrow(dataMatrix)),FillColor = rep(tempColor,nrow(dataMatrix)))
  tempMatrix$Color <- 0
  tempMatrix$Color[selectIDs_1] <- 1
  tempMatrix$Color[selectIDs_2] <- 2
  disCoeffMatrix <- rbind(disCoeffMatrix,tempMatrix)
}
# Draw
Fig <- ggplot()+
  geom_hline(mapping = aes(yintercept = 0),linewidth = 0.3)+
  geom_violinhalf(data = disCoeffMatrix,mapping = aes(x = X,y = Coeff,group = factor(Class),color = factor(FillColor),fill = factor(FillColor)),linewidth = 0.2,alpha = 0.4,scale = 'width',width = 0.7)+
  geom_jitter(data = disCoeffMatrix,mapping = aes(x = X - offset*2,y = Coeff,group = factor(Class),color = factor(Color)),size = 0.03,width = 0.1,alpha = 0.4)+
  scale_x_continuous(limits = c(0.5,9.7),breaks = seq(1,9),labels = ecoVars[seq(9,1,-1)],expand = c(0,0))+
  scale_y_continuous(limits = c(-0.45,0.45),breaks = c(-0.3,0,0.3),labels = c('-0.3','0','0.3'),expand = c(0,0))+
  scale_fill_manual(values = c('#5469af','#bd494b'))+
  scale_color_manual(values = c('#5469af','#bd494b'))+
  coord_flip()+
  theme_custom()
outputFileName <- 'Low_Stable_TEM.pdf'
pdf(file <- outputFileName,width = 1.94,height = 2.8)
print(Fig)
dev.off()
##==== TEM ====##

##==== PR ====##
disCoeffMatrix <- data.frame()
for(i in seq(1,length(ecoVars))){
  fileName <- paste0('./Data/',ecoVars[i],'_PR/Low_Stable_State.csv')
  dataMatrix <- read.csv(fileName)
  dataMatrix <- dataMatrix[seq(1,nrow(dataMatrix)-1),]
  selectIDs_1 <- which(dataMatrix$DiversityCoeff > 0)
  selectIDs_2 <- which(dataMatrix$DiversityCoeff < 0)
  if(length(selectIDs_1) > length(selectIDs_2)){
    tempColor <- 1
  }else{
    tempColor <- 2
  }
  tempMatrix <- data.frame(X = length(ecoVars)+1-i+offset,Coeff = as.numeric(dataMatrix$DiversityCoeff),Class = rep(length(ecoVars)+1-i,nrow(dataMatrix)),FillColor = rep(tempColor,nrow(dataMatrix)))
  tempMatrix$Color <- 0
  tempMatrix$Color[selectIDs_1] <- 1
  tempMatrix$Color[selectIDs_2] <- 2
  disCoeffMatrix <- rbind(disCoeffMatrix,tempMatrix)
}
# Draw
Fig <- ggplot()+
  geom_hline(mapping = aes(yintercept = 0),linewidth = 0.3)+
  geom_violinhalf(data = disCoeffMatrix,mapping = aes(x = X,y = Coeff,group = factor(Class),color = factor(FillColor),fill = factor(FillColor)),linewidth = 0.2,alpha = 0.4,scale = 'width',width = 0.7)+
  geom_jitter(data = disCoeffMatrix,mapping = aes(x = X - offset*2,y = Coeff,group = factor(Class),color = factor(Color)),size = 0.03,width = 0.1,alpha = 0.4)+
  scale_x_continuous(limits = c(0.5,9.7),breaks = seq(1,9),labels = ecoVars[seq(9,1,-1)],expand = c(0,0))+
  scale_y_continuous(limits = c(-0.4,0.4),breaks = c(-0.3,0,0.3),labels = c('-0.3','0','0.3'),expand = c(0,0))+
  scale_fill_manual(values = c('#5469af','#bd494b'))+
  scale_color_manual(values = c('#5469af','#bd494b'))+
  coord_flip()+
  theme_custom()+
  theme(axis.line.y.left = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.text.y = element_blank())
outputFileName <- 'Low_Stable_PR.pdf'
pdf(file <- outputFileName,width = 1.07,height = 2.8)
print(Fig)
dev.off()
##==== PR ====##

##==== VPD ====##
disCoeffMatrix <- data.frame()
for(i in seq(1,length(ecoVars))){
  fileName <- paste0('./Data/',ecoVars[i],'_VPD/Low_Stable_State.csv')
  dataMatrix <- read.csv(fileName)
  dataMatrix <- dataMatrix[seq(1,nrow(dataMatrix)-1),]
  selectIDs_1 <- which(dataMatrix$DiversityCoeff > 0)
  selectIDs_2 <- which(dataMatrix$DiversityCoeff < 0)
  if(length(selectIDs_1) > length(selectIDs_2)){
    tempColor <- 1
  }else{
    tempColor <- 2
  }
  tempMatrix <- data.frame(X = length(ecoVars)+1-i+offset,Coeff = as.numeric(dataMatrix$DiversityCoeff),Class = rep(length(ecoVars)+1-i,nrow(dataMatrix)),FillColor = rep(tempColor,nrow(dataMatrix)))
  tempMatrix$Color <- 0
  tempMatrix$Color[selectIDs_1] <- 1
  tempMatrix$Color[selectIDs_2] <- 2
  disCoeffMatrix <- rbind(disCoeffMatrix,tempMatrix)
}
# Draw
Fig <- ggplot()+
  geom_hline(mapping = aes(yintercept = 0),linewidth = 0.3)+
  geom_violinhalf(data = disCoeffMatrix,mapping = aes(x = X,y = Coeff,group = factor(Class),color = factor(FillColor),fill = factor(FillColor)),linewidth = 0.2,alpha = 0.4,scale = 'width',width = 0.7)+
  geom_jitter(data = disCoeffMatrix,mapping = aes(x = X - offset*2,y = Coeff,group = factor(Class),color = factor(Color)),size = 0.03,width = 0.1,alpha = 0.4)+
  scale_x_continuous(limits = c(0.5,9.7),breaks = seq(1,9),labels = ecoVars[seq(9,1,-1)],expand = c(0,0))+
  scale_y_continuous(limits = c(-0.45,0.45),breaks = c(-0.3,0,0.3),labels = c('-0.3','0','0.3'),expand = c(0,0))+
  scale_fill_manual(values = c('#5469af','#bd494b'))+
  scale_color_manual(values = c('#5469af','#bd494b'))+
  coord_flip()+
  theme_custom()+
  theme(axis.line.y.left = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.text.y = element_blank())
outputFileName <- 'Low_Stable_VPD.pdf'
pdf(file <- outputFileName,width = 1.07,height = 2.8)
print(Fig)
dev.off()
##==== VPD ====##

##==== AI ====##
disCoeffMatrix <- data.frame()
for(i in seq(1,length(ecoVars))){
  fileName <- paste0('./Data/',ecoVars[i],'_AI/Low_Stable_State.csv')
  dataMatrix <- read.csv(fileName)
  dataMatrix <- dataMatrix[seq(1,nrow(dataMatrix)-1),]
  selectIDs_1 <- which(dataMatrix$DiversityCoeff > 0)
  selectIDs_2 <- which(dataMatrix$DiversityCoeff < 0)
  if(length(selectIDs_1) > length(selectIDs_2)){
    tempColor <- 1
  }else{
    tempColor <- 2
  }
  tempMatrix <- data.frame(X = length(ecoVars)+1-i+offset,Coeff = as.numeric(dataMatrix$DiversityCoeff),Class = rep(length(ecoVars)+1-i,nrow(dataMatrix)),FillColor = rep(tempColor,nrow(dataMatrix)))
  tempMatrix$Color <- 0
  tempMatrix$Color[selectIDs_1] <- 1
  tempMatrix$Color[selectIDs_2] <- 2
  disCoeffMatrix <- rbind(disCoeffMatrix,tempMatrix)
}
# Draw
Fig <- ggplot()+
  geom_hline(mapping = aes(yintercept = 0),linewidth = 0.3)+
  geom_violinhalf(data = disCoeffMatrix,mapping = aes(x = X,y = Coeff,group = factor(Class),color = factor(FillColor),fill = factor(FillColor)),linewidth = 0.2,alpha = 0.4,scale = 'width',width = 0.7)+
  geom_jitter(data = disCoeffMatrix,mapping = aes(x = X - offset*2,y = Coeff,group = factor(Class),color = factor(Color)),size = 0.03,width = 0.1,alpha = 0.4)+
  scale_x_continuous(limits = c(0.5,9.7),breaks = seq(1,9),labels = ecoVars[seq(9,1,-1)],expand = c(0,0))+
  scale_y_continuous(limits = c(-0.4,0.4),breaks = c(-0.3,0,0.3),labels = c('-0.3','0','0.3'),expand = c(0,0))+
  scale_fill_manual(values = c('#5469af','#bd494b'))+
  scale_color_manual(values = c('#5469af','#bd494b'))+
  coord_flip()+
  theme_custom()+
  theme(axis.line.y.left = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.text.y = element_blank())
outputFileName <- 'Low_Stable_AI.pdf'
pdf(file <- outputFileName,width = 1.07,height = 2.8)
print(Fig)
dev.off()
##==== AI ====##

##==== PDSI ====##
disCoeffMatrix <- data.frame()
for(i in seq(1,length(ecoVars))){
  fileName <- paste0('./Data/',ecoVars[i],'_PDSI/Low_Stable_State.csv')
  dataMatrix <- read.csv(fileName)
  dataMatrix <- dataMatrix[seq(1,nrow(dataMatrix)-1),]
  selectIDs_1 <- which(dataMatrix$DiversityCoeff > 0)
  selectIDs_2 <- which(dataMatrix$DiversityCoeff < 0)
  if(length(selectIDs_1) > length(selectIDs_2)){
    tempColor <- 1
  }else{
    tempColor <- 2
  }
  tempMatrix <- data.frame(X = length(ecoVars)+1-i+offset,Coeff = as.numeric(dataMatrix$DiversityCoeff),Class = rep(length(ecoVars)+1-i,nrow(dataMatrix)),FillColor = rep(tempColor,nrow(dataMatrix)))
  tempMatrix$Color <- 0
  tempMatrix$Color[selectIDs_1] <- 1
  tempMatrix$Color[selectIDs_2] <- 2
  disCoeffMatrix <- rbind(disCoeffMatrix,tempMatrix)
}
# Draw
Fig <- ggplot()+
  geom_hline(mapping = aes(yintercept = 0),linewidth = 0.3)+
  geom_violinhalf(data = disCoeffMatrix,mapping = aes(x = X,y = Coeff,group = factor(Class),color = factor(FillColor),fill = factor(FillColor)),linewidth = 0.2,alpha = 0.4,scale = 'width',width = 0.7)+
  geom_jitter(data = disCoeffMatrix,mapping = aes(x = X - offset*2,y = Coeff,group = factor(Class),color = factor(Color)),size = 0.03,width = 0.1,alpha = 0.4)+
  scale_x_continuous(limits = c(0.5,9.7),breaks = seq(1,9),labels = ecoVars[seq(9,1,-1)],expand = c(0,0))+
  scale_y_continuous(limits = c(-0.4,0.4),breaks = c(-0.3,0,0.3),labels = c('-0.3','0','0.3'),expand = c(0,0))+
  scale_fill_manual(values = c('#5469af','#bd494b'))+
  scale_color_manual(values = c('#5469af','#bd494b'))+
  coord_flip()+
  theme_custom()+
  theme(axis.line.y.left = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.text.y = element_blank())
outputFileName <- 'Low_Stable_PDSI.pdf'
pdf(file <- outputFileName,width = 1.07,height = 2.8)
print(Fig)
dev.off()
##==== PDSI ====##