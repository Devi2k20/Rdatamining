add<- function(x,y){ x+y }

above10<- function(x){
  use<-x>10
  x[use]
}

above<- function(x ,n){
  use<- x>n
  x[use]
}

columnmean<-function(x, removeNA= TRUE){
  nc<-ncol(x)
  print(show(nc))
  means<-numeric(nc)
 print(typeof(means)) 
  for(i in 1:nc){
    means[i]<-mean(x[, i], na.rm = removeNA)
  }
  means
}