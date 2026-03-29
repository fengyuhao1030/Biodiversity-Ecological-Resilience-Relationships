rm(list = ls())
library(rstudioapi)
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
                   axis.ticks = element_line(linewidth = 0.4),
                   axis.ticks.length = unit(-0.15,'lines'),
                   axis.line.x.bottom = element_line(linewidth = 0.7,arrow = arrow(length = unit(0.03,'inches'),ends = 'last',type = 'closed')),
                   axis.line.y.left = element_line(linewidth = 0.7,arrow = arrow(length = unit(0.03,'inches'),ends = 'last',type = 'closed')),
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 8,margin = margin(0,2,0,0),color = '#000000'),
                   axis.text.x = element_text(size = 8,margin = margin(2,0,0,0),color = '#000000'))
  return(myTheme)
}
##==== Function ====##

X <- seq(0,1,length.out = 100)
Y1 <- 3*X
Y2 <- X
Y3 <- 0.25*X
drawMatrix_1 <- data.frame(X = X,Y = Y1)
selectIDs <- which(drawMatrix_1$X <= 0.4)
drawMatrix_1 <- drawMatrix_1[selectIDs,]
drawMatrix_2 <- data.frame(X = X,Y = Y2)
selectIDs <- which(drawMatrix_2$X <= 0.9)
drawMatrix_2 <- drawMatrix_2[selectIDs,]
drawMatrix_3 <- data.frame(X = X,Y = Y3)

Fig <- ggplot()+
  geom_line(data = drawMatrix_1,mapping = aes(x = X,y = Y),color = '#345bad',linewidth = 0.5)+
  geom_line(data = drawMatrix_2,mapping = aes(x = X,y = Y),color = '#698bd2',linewidth = 0.5)+
  geom_line(data = drawMatrix_3,mapping = aes(x = X,y = Y),color = '#a9bfed',linewidth = 0.5)+
  scale_x_continuous(limits = c(0,1.1),breaks = c(0,1),labels = c('0','K'))+
  scale_y_continuous(limits = c(0,1.2),breaks = c(0),labels = c('0'))+
  theme_custom()
outputFileName <- 'Fig_d-1.pdf'
pdf(file <- outputFileName,width = 0.8,height = 0.77)
print(Fig)
dev.off()

X <- seq(0,1,length.out = 100)
r <- 0.4
K <- 1
c <- 0.2
nugget <- 0.006
K0 <- 1-nugget/(r*(1-c))
Y <- r*X/K*(K0-X/K)*(X/K-c) + nugget
drawMatrix <- data.frame(X = X,Y = Y)

Fig <- ggplot()+
  geom_line(data = drawMatrix,mapping = aes(x = X,y = Y),color = '#961717',linewidth = 0.5)+
  scale_x_continuous(limits = c(0,1.1),breaks = c(0,1),labels = c('0','K'))+
  scale_y_continuous(limits = c(-0.0001,0.05),breaks = c(0),labels = c('0'))+
  theme_custom()
outputFileName <- 'Fig_d-2.pdf'
pdf(file <- outputFileName,width = 0.8,height = 0.77)
print(Fig)
dev.off()