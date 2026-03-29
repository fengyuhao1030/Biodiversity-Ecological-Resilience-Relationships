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

ecoVars <- c('GIMMSKNDVI','PKUKNDVI','LUEGPP','MUSESGPP','NIRVGPP','CASAGIMMS','CASAPKU','MUSESNPP','VODCA')

##==== TEM ====##
hysteresisCoeffMatrix <- data.frame()
for(i in seq(1,length(ecoVars))){
  fileName <- paste0('./Data/',ecoVars[i],'_TEM/Bistable_States.csv')
  dataMatrix <- read.csv(fileName)
  tempMatrix <- data.frame(X = length(ecoVars)+1-i,Coeff = as.numeric(dataMatrix$DiversityCoeff[1]))
  tempMatrix$Color <- 0
  if(as.numeric(tempMatrix$Coeff[1]) > 0){
    tempMatrix$Color <- 1
  }else{
    tempMatrix$Color <- 2
  }
  hysteresisCoeffMatrix <- rbind(hysteresisCoeffMatrix,tempMatrix)
}
# Draw
Fig <- ggplot()+
  geom_hline(mapping = aes(yintercept = 0),linewidth = 0.3)+
  geom_col(data = hysteresisCoeffMatrix,mapping = aes(x = X,y = Coeff,fill = factor(Color)),width = 0.55,linewidth = 0.3,color = '#000000')+
  scale_x_continuous(limits = c(0.5,9.5),breaks = seq(1,9),labels = ecoVars[seq(9,1,-1)],expand = c(0,0))+
  scale_y_continuous(limits = c(-0.58,0.58),breaks = c(-0.3,0,0.3),labels = c('-0.3','0','0.3'),expand = c(0,0))+
  scale_fill_manual(values = c('#5469af','#bd494b'))+
  coord_flip()+
  theme_custom()
outputFileName <- 'Bi_Stables_TEM.pdf'
pdf(file <- outputFileName,width = 1.94,height = 2.8)
print(Fig)
dev.off()
##==== TEM ====##

##==== PR ====##
hysteresisCoeffMatrix <- data.frame()
for(i in seq(1,length(ecoVars))){
  fileName <- paste0('./Data/',ecoVars[i],'_PR/Bistable_States.csv')
  dataMatrix <- read.csv(fileName)
  tempMatrix <- data.frame(X = length(ecoVars)+1-i,Coeff = as.numeric(dataMatrix$DiversityCoeff[1]))
  tempMatrix$Color <- 0
  if(as.numeric(tempMatrix$Coeff[1]) > 0){
    tempMatrix$Color <- 1
  }else{
    tempMatrix$Color <- 2
  }
  hysteresisCoeffMatrix <- rbind(hysteresisCoeffMatrix,tempMatrix)
}
# Draw
Fig <- ggplot()+
  geom_hline(mapping = aes(yintercept = 0),linewidth = 0.3)+
  geom_col(data = hysteresisCoeffMatrix,mapping = aes(x = X,y = Coeff,fill = factor(Color)),width = 0.55,linewidth = 0.3,color = '#000000')+
  scale_x_continuous(limits = c(0.5,9.5),breaks = seq(1,9),labels = ecoVars[seq(9,1,-1)],expand = c(0,0))+
  scale_y_continuous(limits = c(-0.4,0.4),breaks = c(-0.2,0,0.2),labels = c('-0.2','0','0.2'),expand = c(0,0))+
  scale_fill_manual(values = c('#5469af','#bd494b'))+
  coord_flip()+
  theme_custom()+
  theme(axis.line.y.left = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.text.y = element_blank())
outputFileName <- 'Bi_Stables_PR.pdf'
pdf(file <- outputFileName,width = 1.07,height = 2.8)
print(Fig)
dev.off()
##==== PR ====##

##==== VPD ====##
hysteresisCoeffMatrix <- data.frame()
for(i in seq(1,length(ecoVars))){
  fileName <- paste0('./Data/',ecoVars[i],'_VPD/Bistable_States.csv')
  dataMatrix <- read.csv(fileName)
  tempMatrix <- data.frame(X = length(ecoVars)+1-i,Coeff = as.numeric(dataMatrix$DiversityCoeff[1]))
  tempMatrix$Color <- 0
  if(as.numeric(tempMatrix$Coeff[1]) > 0){
    tempMatrix$Color <- 1
  }else{
    tempMatrix$Color <- 2
  }
  hysteresisCoeffMatrix <- rbind(hysteresisCoeffMatrix,tempMatrix)
}
# Draw
Fig <- ggplot()+
  geom_hline(mapping = aes(yintercept = 0),linewidth = 0.3)+
  geom_col(data = hysteresisCoeffMatrix,mapping = aes(x = X,y = Coeff,fill = factor(Color)),width = 0.55,linewidth = 0.3,color = '#000000')+
  scale_x_continuous(limits = c(0.5,9.5),breaks = seq(1,9),labels = ecoVars[seq(9,1,-1)],expand = c(0,0))+
  scale_y_continuous(limits = c(-0.5,0.5),breaks = c(-0.3,0,0.3),labels = c('-0.3','0','0.3'),expand = c(0,0))+
  scale_fill_manual(values = c('#5469af','#bd494b'))+
  coord_flip()+
  theme_custom()+
  theme(axis.line.y.left = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.text.y = element_blank())
outputFileName <- 'Bi_Stables_VPD.pdf'
pdf(file <- outputFileName,width = 1.07,height = 2.8)
print(Fig)
dev.off()
##==== VPD ====##

##==== AI ====##
hysteresisCoeffMatrix <- data.frame()
for(i in seq(1,length(ecoVars))){
  fileName <- paste0('./Data/',ecoVars[i],'_AI/Bistable_States.csv')
  dataMatrix <- read.csv(fileName)
  tempMatrix <- data.frame(X = length(ecoVars)+1-i,Coeff = as.numeric(dataMatrix$DiversityCoeff[1]))
  tempMatrix$Color <- 0
  if(as.numeric(tempMatrix$Coeff[1]) > 0){
    tempMatrix$Color <- 1
  }else{
    tempMatrix$Color <- 2
  }
  hysteresisCoeffMatrix <- rbind(hysteresisCoeffMatrix,tempMatrix)
}
# Draw
Fig <- ggplot()+
  geom_hline(mapping = aes(yintercept = 0),linewidth = 0.3)+
  geom_col(data = hysteresisCoeffMatrix,mapping = aes(x = X,y = Coeff,fill = factor(Color)),width = 0.55,linewidth = 0.3,color = '#000000')+
  scale_x_continuous(limits = c(0.5,9.5),breaks = seq(1,9),labels = ecoVars[seq(9,1,-1)],expand = c(0,0))+
  scale_y_continuous(limits = c(-0.4,0.4),breaks = c(-0.2,0,0.2),labels = c('-0.2','0','0.2'),expand = c(0,0))+
  scale_fill_manual(values = c('#5469af','#bd494b'))+
  coord_flip()+
  theme_custom()+
  theme(axis.line.y.left = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.text.y = element_blank())
outputFileName <- 'Bi_Stables_AI.pdf'
pdf(file <- outputFileName,width = 1.07,height = 2.8)
print(Fig)
dev.off()
##==== AI ====##

##==== PDSI ====##
hysteresisCoeffMatrix <- data.frame()
for(i in seq(1,length(ecoVars))){
  fileName <- paste0('./Data/',ecoVars[i],'_PDSI/Bistable_States.csv')
  dataMatrix <- read.csv(fileName)
  tempMatrix <- data.frame(X = length(ecoVars)+1-i,Coeff = as.numeric(dataMatrix$DiversityCoeff[1]))
  tempMatrix$Color <- 0
  if(as.numeric(tempMatrix$Coeff[1]) > 0){
    tempMatrix$Color <- 1
  }else{
    tempMatrix$Color <- 2
  }
  hysteresisCoeffMatrix <- rbind(hysteresisCoeffMatrix,tempMatrix)
}
# Draw
Fig <- ggplot()+
  geom_hline(mapping = aes(yintercept = 0),linewidth = 0.3)+
  geom_col(data = hysteresisCoeffMatrix,mapping = aes(x = X,y = Coeff,fill = factor(Color)),width = 0.55,linewidth = 0.3,color = '#000000')+
  scale_x_continuous(limits = c(0.5,9.5),breaks = seq(1,9),labels = ecoVars[seq(9,1,-1)],expand = c(0,0))+
  scale_y_continuous(limits = c(-0.5,0.5),breaks = c(-0.3,0,0.3),labels = c('-0.3','0','0.3'),expand = c(0,0))+
  scale_fill_manual(values = c('#5469af','#bd494b'))+
  coord_flip()+
  theme_custom()+
  theme(axis.line.y.left = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.text.y = element_blank())
outputFileName <- 'Bi_Stables_PDSI.pdf'
pdf(file <- outputFileName,width = 1.07,height = 2.8)
print(Fig)
dev.off()
##==== PDSI ====##