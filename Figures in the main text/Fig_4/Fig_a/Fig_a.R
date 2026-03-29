rm(list = ls())
library(rstudioapi)
library(ggplot2)
library(terra)
currentPath <- getSourceEditorContext()$path
charLocations <- gregexpr('/',currentPath)[[1]]
currentPath <- substring(currentPath,1,charLocations[length(charLocations)]-1)
setwd(currentPath)

refRaster <- rast(nrows = 2160,ncols = 4320,xmin = -180,xmax = 180,ymin = -90,ymax = 90,crs = 'EPSG:4326')

load('./GIMMSKNDVI_TEM/High_Stable_State_Mean.RData')
highMatrix <- meanDataMatrix
highMatrix$Class <- highMatrix$Type
highMatrix$Type <- 1
highMatrix <- highMatrix[,c('X','Y','Type','Class')]
load('./GIMMSKNDVI_TEM/Low_Stable_State_Mean.RData')
lowMatrix <- meanDataMatrix
lowMatrix$Class <- lowMatrix$Type
lowMatrix$Type <- 2
lowMatrix <- lowMatrix[,c('X','Y','Type','Class')]
load('./GIMMSKNDVI_TEM/Bistable_States.RData')
shiftMatrix <- dataMatrix
shiftMatrix$Class <- shiftMatrix$Type
shiftMatrix$Type <- 3
shiftMatrix <- shiftMatrix[,c('X','Y','Type','Class')]
dataMatrix <- rbind(highMatrix,lowMatrix,shiftMatrix)
write.csv(dataMatrix,file = 'Type_Class.csv',row.names = FALSE)

# Output
outputRaster <- rast(dataMatrix[,c(1,2,3)],type = 'xyz',crs = 'EPSG:4326')
outputRaster <- resample(outputRaster,refRaster,method = 'near')
writeRaster(outputRaster,filename = 'Type.tif',overwrite = TRUE)