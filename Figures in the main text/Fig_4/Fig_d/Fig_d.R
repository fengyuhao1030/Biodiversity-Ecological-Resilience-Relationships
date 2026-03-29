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

load('./GIMMSKNDVI_TEM/Bistable_States.RData')
# Line
lineCoeff <- read.csv('./GIMMSKNDVI_TEM/Bistable_States.csv')
tempX <- seq(min(dataMatrix$Diversity),max(dataMatrix$Diversity),length.out = 100)
tempY <- as.numeric(lineCoeff[1])*tempX + as.numeric(lineCoeff[2])
lineMatrix <- data.frame(X = tempX,Y = tempY)

# Draw
Fig <- ggplot()+
  geom_point(data = dataMatrix,mapping = aes(x = Diversity,y = Hysteresis),size = 0.4,color = '#999999',alpha = 0.6)+
  geom_line(data = lineMatrix,mapping = aes(x = X,y = Y),color = '#000000',linewidth = 0.7)+
  scale_x_continuous(breaks = c(0,20,40,60))+
  scale_y_continuous(limits = c(-0.04,1.05),breaks = seq(0,1,0.25),labels = c('0','25','50','75','100'),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_d.pdf'
pdf(file <- outputFileName,width = 1.9,height = 1.75)
print(Fig)
dev.off()