s1 <- read_sf("Indian_States.shp")

library(tmap)
tm_shape(s1) + tm_polygons(style="quantile", col = "confirmed") +
  tm_legend(outside = TRUE, text.size = .8) 
----------------------------------------------
neighbours <- poly2nb(s1)
neighbours
listw <- nb2listw(neighbours, style="W", zero.policy=TRUE)
globalMoran <- moran.test(s1$confirmed, listw, zero.policy=TRUE, na.action=na.omit)
globalMoran
listw
local <- localmoran(x = s1$confirmed, listw )#= nb2listw(neighbours, style = "W", zero.policy=TRUE, NAOK = NAOK))
moran.map <- cbind(s1, local)
tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          title = "local moran statistic")
-----------------------------------------------
  quadrant <- vector(mode="numeric",length=nrow(local))

# centers the variable of interest around its mean
m.qualification <- s1$confirmed - mean(s1$confirmed)     

# centers the local Moran's around the mean
m.local <- local[,1] - mean(local[,1])    

# significance threshold
signif <- 0.3 

# builds a data quadrant
quadrant[m.qualification >0 & m.local>0] <- 4  
quadrant[m.qualification <0 & m.local<0] <- 1      
quadrant[m.qualification <0 & m.local>0] <- 2
quadrant[m.qualification >0 & m.local<0] <- 3
quadrant[local[,5]>signif] <- 0   

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(s1$confirmed,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
box()
legend("bottomleft", legend = c("insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n")
--------------------------------------------------------------
library(spdep)
nb <- poly2nb(s1, queen=TRUE)
nb[[5]]
#lw1<- spweights.constants(lw, zero.policy=TRUE, adjust.n=TRUE)

lw <- nb2listw(nb, style="W", zero.policy=TRUE)
Inc.lag <- lag.listw(lw, s1$confirmed)

M <- lm(Inc.lag ~ s1$confirmed)
plot( Inc.lag ~ s1$confirmed, pch=20, asp=1, las=1)
coef(M)[2]

n <- 599L   # Define the number of simulations
I.r <- vector(length=n)  # Create an empty vector

for (i in 1:n){
  # Randomly shuffle income values
  x <- sample(s1$confirmed, replace=FALSE)
  # Compute new set of lagged values
  x.lag <- lag.listw(lw, x)
  # Compute the regression slope and store its value
  M.r    <- lm(x.lag ~ x)
  I.r[i] <- coef(M.r)[2]
}

hist(I.r, main=NULL, xlab="Moran's I", las=1)
abline(v=coef(M)[2], col="red")
##
N.greater <- sum(coef(M)[2] > I.r)

p <- min(N.greater + 1, n + 1 - N.greater) / (n + 1)
p
#####
moran.test(s1$confirmed,lw, zero.policy=TRUE, na.action=na.omit)

MC<- moran.mc(s1$confirmed, lw, nsim=599,zero.policy=TRUE, na.action=na.omit)
plot(MC, main="", las=1)
###########
coo <- coordinates(s1)


