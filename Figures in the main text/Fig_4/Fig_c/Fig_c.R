rm(list = ls())
library(rstudioapi)
library(ggplot2)
currentPath <- getSourceEditorContext()$path
charLocations <- gregexpr('/',currentPath)[[1]]
currentPath <- substring(currentPath,1,charLocations[length(charLocations)]-1)
setwd(currentPath)

##==== Function ====##
theme_custom <- function(){
  myTheme <- theme(panel.background = element_rect(fill = 'white',color = 'black',linewidth = 0.7),
                   panel.grid = element_blank(),
                   legend.position = 'none',
                   plot.margin = margin(3,3,3,3),
                   plot.background = element_blank(),
                   axis.ticks = element_line(linewidth = 0.3),
                   axis.ticks.length.x.bottom = unit(-0.15,'lines'),
                   axis.ticks.length.y.left = unit(-0.15,'lines'),
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 8,margin = margin(0,3,0,0),color = '#000000'),
                   axis.text.x = element_text(size = 8,margin = margin(3,0,0,0),color = '#000000'))
  return(myTheme)
}
##==== Function ====##

dataMatrix <- read.csv('./GIMMSKNDVI_TEM/Low_Stable_State.csv')
dataMatrix <- dataMatrix[seq(1,nrow(dataMatrix)-1),]

dataMatrix$X <- 0
dataMatrix$Y <- 0
dataMatrix$Color <- 0
breaks <- seq(-0.04,0.22,0.005)
for(i in seq(1,length(breaks)-1)){
  downLimit <- breaks[i]
  upLimit <- breaks[i+1]
  tempX <- (downLimit + upLimit)/2
  selectIDs <- which((dataMatrix$DiversityCoeff > downLimit) & (dataMatrix$DiversityCoeff < upLimit))
  dataMatrix$X[selectIDs] <- tempX
  dataMatrix$Y[selectIDs] <- seq(1,length(selectIDs))
  if(tempX < 0){
    dataMatrix$Color[selectIDs] <- 1
  }else{
    dataMatrix$Color[selectIDs] <- 2
  }
}

# Draw
Fig <- ggplot()+
  geom_vline(mapping = aes(xintercept = 0),linewidth = 0.4,color = '#000000')+
  geom_point(data = dataMatrix,mapping = aes(x = X,y = Y),color = '#5469af',size = 0.2)+
  scale_x_continuous(limits = c(-0.04,0.23),breaks = c(0,0.1,0.2),labels = c('0','0.1','0.2'),expand = c(0,0))+
  scale_y_continuous(limits = c(-1.6,55),breaks = seq(0,40,20),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_c.pdf'
pdf(file <- outputFileName,width = 1.84,height = 1.75)
print(Fig)
dev.off()