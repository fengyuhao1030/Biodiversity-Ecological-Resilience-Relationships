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

X <- seq(0,1,length.out = 100)
r <- 0.6
K <- 1
c <- 0.2
nugget <- 0.01
K0 <- 1-nugget/(r*(1-c))
Y <- r*X/K*(K0-X/K)*(X/K-c) + nugget
drawMatrix <- data.frame(X = X,Y = Y)

Fig <- ggplot()+
  geom_line(data = drawMatrix,mapping = aes(x = X,y = Y),color = '#961717',linewidth = 0.8)+
  scale_x_continuous(limits = c(-0.0667,1.0667),breaks = c(0,0.2,1),labels = c('0','c','K'),expand = c(0,0))+
  scale_y_continuous(limits = c(-0.00015,0.075),breaks = c(0,nugget),labels = c('0','v'))+
  theme_custom()
outputFileName <- 'Fig_b.pdf'
pdf(file <- outputFileName,width = 1.7,height = 1.5)
print(Fig)
dev.off()