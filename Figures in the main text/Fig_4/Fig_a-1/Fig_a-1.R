rm(list = ls())
library(rstudioapi)
library(terra)
library(ggplot2)
currentPath <- getSourceEditorContext()$path
charLocations <- gregexpr('/',currentPath)[[1]]
currentPath <- substring(currentPath,1,charLocations[length(charLocations)]-1)
setwd(currentPath)

typeRaster <- rast('Type_Proj.tif')
typeRasterCoords <- as.data.frame(crds(typeRaster,na.rm = FALSE))
typeRasterValues <- as.data.frame(values(typeRaster))
typeRasterMatrix <- cbind(typeRasterCoords,typeRasterValues)
colnames(typeRasterMatrix) <- c('X','Y','Type')
selectIDs <- which(!is.na(typeRasterMatrix$Type))
typeRasterMatrix <- typeRasterMatrix[selectIDs,]

##==== Inner ====##
selectIDs_1 <- which(typeRasterMatrix$Type == 100)
selectIDs_2 <- which(typeRasterMatrix$Type == 200)
selectIDs_3 <- which(typeRasterMatrix$Type == 300)
allNum <- length(selectIDs_1) + length(selectIDs_2) + length(selectIDs_3)
tempMatrix_1 <- data.frame(Class = 1,Value = length(selectIDs_1)/allNum)
tempMatrix_2 <- data.frame(Class = 2,Value = length(selectIDs_2)/allNum)
tempMatrix_3 <- data.frame(Class = 3,Value = length(selectIDs_3)/allNum)
drawMatrix_1 <- rbind(tempMatrix_1,tempMatrix_2,tempMatrix_3)
drawMatrix_1$ValueMax <- cumsum(drawMatrix_1$Value)
drawMatrix_1$ValueMin <- c(0,head(drawMatrix_1$ValueMax,n = -1))
##==== Inner ====##

##==== Outer ====##
selectIDs_1 <- which(typeRasterMatrix$Type == 2)
selectIDs_2 <- which(typeRasterMatrix$Type == 100)
selectIDs_3 <- which(typeRasterMatrix$Type == 200)
selectIDs_4 <- which(typeRasterMatrix$Type == 300)
allNum <- length(selectIDs_1) + length(selectIDs_2) + length(selectIDs_3) + length(selectIDs_4)
tempMatrix_1 <- data.frame(Class = 4,Value = length(selectIDs_1)/allNum)
tempMatrix_2 <- data.frame(Class = 5,Value = length(selectIDs_2)/allNum)
tempMatrix_3 <- data.frame(Class = 6,Value = length(selectIDs_3)/allNum)
tempMatrix_4 <- data.frame(Class = 7,Value = length(selectIDs_4)/allNum)
drawMatrix_2 <- rbind(tempMatrix_1,tempMatrix_2,tempMatrix_3,tempMatrix_4)
drawMatrix_2$ValueMax <- cumsum(drawMatrix_2$Value)
drawMatrix_2$ValueMin <- c(0,head(drawMatrix_2$ValueMax,n = -1))
##==== Outer ====##

##==== Draw ====##
Fig <- ggplot()+
  geom_rect(data = drawMatrix_1,mapping = aes(xmin = 0,xmax = 3,ymin = ValueMin,ymax = ValueMax,fill = factor(Class)),color = '#333333',linewidth = 0.1)+
  geom_rect(data = drawMatrix_2,mapping = aes(xmin = 4,xmax = 5.5,ymin = ValueMin,ymax = ValueMax,fill = factor(Class)),color = '#333333',linewidth = 0.1)+
  scale_fill_manual(values = c('#6aa92b','#dda930','#60c1e1','#343434','#6aa92b','#dda930','#60c1e1'))+
  coord_polar(theta = 'y')+
  scale_x_continuous(limits = c(0,5.5))+
  theme_void()+
  theme(legend.position = 'none')
outputFileName <- 'Fig_a-1.pdf'
pdf(file <- outputFileName,width = 1.3,height = 1.3)
print(Fig)
dev.off()
##==== Draw ====##