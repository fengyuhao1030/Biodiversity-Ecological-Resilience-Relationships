rm(list = ls())
library(rstudioapi)
library(ggplot2)
library(raster)
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
                   axis.ticks.length = unit(-0.2,'lines'),
                   axis.line.x.bottom = element_line(linewidth = 0.7,arrow = arrow(length = unit(0.05,'inches'),ends = 'last',type = 'closed')),
                   axis.line.y.left = element_line(linewidth = 0.7,arrow = arrow(length = unit(0.05,'inches'),ends = 'last',type = 'closed')),
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 8,margin = margin(0,4,0,0),color = '#000000'),
                   axis.text.x = element_text(size = 8,margin = margin(4,0,0,0),color = '#000000'))
  return(myTheme)
}
##==== Function ====##

dataMatrix <- raster('BioEffect.tif')
zeroContour <- coordinates(rasterToContour(dataMatrix,levels = 0))
zeroContour <- as.data.frame(zeroContour[[1]][[1]])
colnames(zeroContour) <- c('X','Y')
dataMatrix <- as.data.frame(dataMatrix,xy = TRUE)

maxValue <- max(dataMatrix$BioEffect)
minValue <- min(dataMatrix$BioEffect)
tempMatrix1 <- data.frame(x = 2,y = 2,BioEffect = maxValue)
tempMatrix2 <- data.frame(x = -2,y = -2,BioEffect = minValue)
custom_colors <- c('#a62826','#d92a27','#f58d38','#b0d154','#48a18b','#80c0e8','#dfeef5')

## Draw
dataMatrix <- rbind(dataMatrix,tempMatrix1,tempMatrix2)
selectIDs <- which(dataMatrix$BioEffect > 0)
dataMatrix$BioEffect[selectIDs] <- dataMatrix$BioEffect[selectIDs]/abs(maxValue)
selectIDs <- which(dataMatrix$BioEffect < 0)
dataMatrix$BioEffect[selectIDs] <- dataMatrix$BioEffect[selectIDs]/abs(minValue)
pointMatrix <- data.frame(X = c(0.8,0.3),Y = c(0.3,0.8))
Fig <- ggplot()+
  geom_raster(data = dataMatrix,mapping = aes(x = x,y = y,fill = BioEffect))+
  geom_segment(mapping = aes(x = 0,y = 0,xend = 1,yend = 1),linetype = '22222222',color = '#000000',linewidth = 0.5)+
  geom_line(data = zeroContour,mapping = aes(x = X,y = Y),color = '#446cbc',linewidth = 0.5)+
  geom_point(data = pointMatrix,mapping = aes(x = X,y = Y),size = 2.5,shape = 21,fill = '#83c24f',color = '#000000',stroke = 0.5)+
  scale_x_continuous(limits = c(-0.06,1.06),breaks = seq(0,1,0.5),labels = c('0','0.5','1.0'),expand = c(0,0))+
  scale_y_continuous(limits = c(-0.06,1.06),breaks = seq(0,1,0.5),labels = c('0','0.5','1.0'),expand = c(0,0))+
  scale_fill_gradientn(colors = custom_colors)+
  theme_custom()
outputFileName <- 'Fig_d.pdf'
pdf(file <- outputFileName,width = 1.75,height = 1.6)
print(Fig)
dev.off()