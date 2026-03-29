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
GenArrow <- function(centerPoint,angle,size){
  # Point 1
  deltaX <- sin(angle/180*pi)*size*0.264
  deltaY <- cos(angle/180*pi)*size
  point1X <- centerPoint[1] + deltaX
  point1Y <- centerPoint[2] + deltaY
  # Point 2
  deltaX <- sin((angle-120)/180*pi)*size*0.264
  deltaY <- cos((angle-120)/180*pi)*size
  point2X <- centerPoint[1] + deltaX
  point2Y <- centerPoint[2] + deltaY
  # Point 3
  deltaX <- sin((angle+120)/180*pi)*size*0.264
  deltaY <- cos((angle+120)/180*pi)*size
  point3X <- centerPoint[1] + deltaX
  point3Y <- centerPoint[2] + deltaY
  # Arrow
  arrowPolygon <- data.frame(X = c(point1X,point2X,point3X,point1X),Y = c(point1Y,point2Y,point3Y,point1Y))
  return(arrowPolygon)
}
##==== Function ====##

stateDynamicModel <- function(t,y,p){
  X <- y[1]
  with(as.list(p),{
    dXdt <- r*X/K*(K0-X/K)*(X/K-c) + nugget - env*X
    return(list(dXdt))
  })
}
# Initialization
r <- 0.6
K <- 1
c <- 0.2
nugget <- 0.01
K0 <- 1-nugget/(r*(1-c))
envs <- seq(0,0.36,length.out = 500)
# Calculate stable solutions
stableStateMatrix <- data.frame()
for(i in seq(1,length(envs))){
  env <- envs[i]
  p <- c(r = r,K = K,K0 = K0,c = c,nugget = nugget,env = env)
  times <- seq(0,1000,0.1)
  state0 <- 1
  modelOut <- ode(y = state0,times = times,func = stateDynamicModel,parms = p,method = 'rk4')
  print(i)
  if(nrow(modelOut) != length(times)){
    next
  }
  stableState <- as.numeric(modelOut[length(times),2])
  if(is.na(stableState)){
    next
  }
  if(stableState > 1){
    next
  }
  if(any(modelOut[,2] < 0)){
    next
  }
  tempDistance <- 3*(r/K^3)*stableState^2 - 2*((r*K0+r*c)/K^2)*stableState + r*c*K0/K + env
  tempMatrix <- data.frame(Env = env,State = stableState,Distance = tempDistance)
  stableStateMatrix <- rbind(stableStateMatrix,tempMatrix)
  print(as.numeric(tempMatrix$State))
}
selectIDs <- which(stableStateMatrix$Distance < 0)
stableStateMatrix$Distance[selectIDs] <- 0
stableStateMatrix$Type <- 0
selectIDs <- which(stableStateMatrix$State > 0.5)
stableStateMatrix$Type[selectIDs] <- 1
stableStateMatrix$Type[-selectIDs] <- 2
tempMatrix <- stableStateMatrix[selectIDs,]
selectID <- which(tempMatrix$State == min(tempMatrix$State))
tippingpoint <- tempMatrix[selectID,]
# Arrow
arrow1 <- GenArrow(c(stableStateMatrix$Env[80],stableStateMatrix$State[80]),120,0.044)
arrow2 <- GenArrow(c(as.numeric(tippingpoint$Env[1]),0.3),180,0.044)
arrow3 <- GenArrow(c(stableStateMatrix$Env[310],stableStateMatrix$State[310]),91,0.044)

##==== Draw ====##
Fig <- ggplot()+
  geom_line(data = stableStateMatrix,mapping = aes(x = Env,y = State,group = factor(Type),color = Distance),linewidth = 1.7)+
  geom_line(data = stableStateMatrix,mapping = aes(x = Env,y = State),color = '#4258b2',linewidth = 0.7)+
  geom_point(data = tippingpoint,mapping = aes(x = Env,y = State),shape = 21,size = 2,fill = '#4258b2',color = '#000000',stroke = 0.4)+
  geom_polygon(data = arrow1,mapping = aes(x = X,y = Y),fill = '#4258b2',color = '#000000',linewidth = 0.25)+
  geom_polygon(data = arrow2,mapping = aes(x = X,y = Y),fill = '#4258b2',color = '#000000',linewidth = 0.25)+
  geom_polygon(data = arrow3,mapping = aes(x = X,y = Y),fill = '#4258b2',color = '#000000',linewidth = 0.25)+
  scale_x_continuous(limits = c(-0.024,0.384),breaks = c(0,0.36),labels = c('0','Max'),expand = c(0,0))+
  scale_y_continuous(limits = c(-0.25,1.2),breaks = c(0,1),labels = c('0','K'),expand = c(0,0))+
  scale_color_gradient(low = '#e61c17',high = '#fbf300')+
  theme_custom()
outputFileName <- 'Fig_c.pdf'
pdf(file <- outputFileName,width = 1.71,height = 1.5)
print(Fig)
dev.off()
# Output data
selectIDs <- which(stableStateMatrix$State > 0.5)
stableStateMatrix <- stableStateMatrix[selectIDs,]
write.csv(stableStateMatrix,file = 'Collapse.csv',row.names = FALSE)
##==== Draw ====##