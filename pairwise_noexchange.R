library(igraph)
library(tidyverse)
library(data.table)

mdt <- matrix(NA, nrow=(num_simulations*duration), ncol=5)
bt <- 1
df <- matrix(NA, nrow=num_simulations, ncol=4)

for(v in 1:num_simulations){
  g <- graph(c(1,2), n=2, directed=FALSE)
  V(g)$req <- 0
  V(g)$dead <- NA
  # create data structures here
  dtr <- matrix(NA, nrow=duration, ncol=((vcount(g)*2)+1))
  dcal <- matrix(NA, nrow=duration, ncol=vcount(g))
  for(i in 1:vcount(g)){
    dtmp <- sample(c(0,1), duration, replace=TRUE, 
                   prob=c((1-vtrate),vtrate))
    dcal[,i] <- dtmp
  }
  V(g)$stock <- init_herdsize
  V(g)$ut <- 0  # under threshold count
  
  # set current time step - for loop in 1:duration here
  for(t in 1:duration){
    
    growth <- (rnorm(vcount(g), mean=0.034, sd=0.0253))
    V(g)$stock <- V(g)$stock + (V(g)$stock*growth)
    tmp <- dcal[t,]
    if(any(tmp==1)){
      V(g)$stock[tmp==1] <- 
        V(g)$stock[tmp==1] - (V(g)$stock[tmp==1]*rnorm(1,mean=0.3,sd=0.1))
    }
    V(g)$stock <- round(V(g)$stock)
    
    # establish need list (nlist)
    #   nlist <- V(g)[(V(g)$stock < minstock) & V(g)$stock != 0]
    #  V(g)$imp <- NA  # no requests yet
    # V(g)$req <- 0
    
    # viability check (ut + 1 if below threshold)
    if(any(V(g)$stock[V(g)$stock != 0] < minstock)){
      V(g)[(V(g)$stock < minstock) & (V(g)$stock != 0)]$ut <- 
        V(g)[(V(g)$stock < minstock) & (V(g)$stock != 0)]$ut + 1
    }
    
    # vertex 'deleted' if ut >= 2 (stock == 0, precludes them from osotua/growth)
    if(any(V(g)$ut >= 2)){
      V(g)[V(g)$ut >= 2]$stock <- 0
      V(g)[V(g)$ut >= 2]$dead <- t
      V(g)[V(g)$ut >= 2]$ut <- 0
      #g <- delete_vertices(g, V(g)[V(g)$ut >= 2])
    }
    
    # boundary checks
    # min check not necessary/completed with viability check
    
    # max check
    if(any(V(g)$stock > maxstock)){
      V(g)[V(g)$stock > maxstock]$stock <- maxstock
    }
    
    # data collection
    dtr[t,1] <- t; dtr[t,2] <- V(g)$stock[1]; dtr[t,3] <- V(g)$stock[2]
    
    # end simulation? if only one partner is standing
    if((vcount(g)) <= length(V(g)$stock[V(g)$stock==0])){
      break
    }
    
    # end simulation condition in Aktipis et al. (2011);
    # 'run until both agents were removed from the population'
    
    #    if(vcount(g)==0){
    #     break
    #  }
    
  }  # simulation part is here
  
  mdt[bt:(bt+(duration-1)),1:5] <- dtr
  bt <- bt + duration
  df[v,] <- c(V(g)$stock, V(g)$dead)
}

