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
##==== Function ====##

dataMatrix <- read.csv('ExampleResults.csv')
medianMatrix <- data.frame()
for(i in seq(1,3)){
  selectIDs <- which(dataMatrix$SpeciesNum == i)
  tempDataMatrix <- dataMatrix[selectIDs,]
  tempDataMatrix <- tempDataMatrix[order(tempDataMatrix$RelativeHysteresis),]
  medianMatrix <- rbind(medianMatrix,tempDataMatrix[51,])
}
selectIDs <- which(dataMatrix$RelativeHysteresis > 0)
dataMatrix <- dataMatrix[selectIDs,]

# Draw
Fig <- ggplot()+
  geom_violin(data = dataMatrix,mapping = aes(x = SpeciesNum,y = RelativeHysteresis,fill = as.factor(SpeciesNum)),color = NA,alpha = 0.7)+
  geom_jitter(data = dataMatrix,mapping = aes(x = SpeciesNum,y = RelativeHysteresis,fill = as.factor(SpeciesNum)),shape = 21,size = 1,stroke = 0.2,color = '#000000',width = 0.2)+
  geom_point(data = medianMatrix,mapping = aes(x = SpeciesNum,y = RelativeHysteresis),shape = 21,size = 1.5,stroke = 0.3,color = '#000000',fill = '#c4101d')+
  scale_x_continuous(limits = c(0.2,3.8),breaks = c(1,2,3),expand = c(0,0))+
  scale_y_continuous(limits = c(0,50),breaks = c(0,20,40),labels = c('0','20','40'),expand = c(0,0))+
  scale_fill_manual(values = c('#76b879','#7e95cf','#7bbadb'))+
  theme_custom()
outputFileName <- 'Fig_b-1.pdf'
pdf(file <- outputFileName,width = 1.58,height = 1.5)
print(Fig)
dev.off()