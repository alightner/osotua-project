library(igraph)
library(tidyverse)
library(data.table)

# defection script (faked need among isotuatin)

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
    nlist <- V(g)[(V(g)$stock < minstock) & V(g)$stock != 0]
    V(g)$imp <- NA  # no requests yet
    V(g)$req <- 0
    
    if(!(V(g)[defect_node] %in% nlist)){
      if(runif(1,0,1) <= dfpr){
        nlist <- c(nlist,V(g)[defect_node])
        V(g)[defect_node]$req <- sample(rlist,1)      # mean and sd?
      }
    }   # defection from vertex defect_node, pr=dfpr
    
    if(length(nlist) > 0){   # request process when applicable
      # randomized nlist order (for larger network)
      nlist <- sample(nlist, length(nlist))
      V(g)$req[V(g)$stock < minstock & V(g)$stock != 0] <- 
        minstock - V(g)$stock[V(g)$stock < minstock & V(g)$stock != 0]
      
      
      # run through the need list, 'imperatives' (imp) are sent
      for(i in nlist){
        cand <- as.numeric(unlist(neighbors(g, V(g)[i])))
        if(length(cand) > 1){  # if multiple, pick one randomly
          cand <- sample(cand, 1)
        }
        V(g)$imp[cand] <- V(g)[i]  # sending request (imperative)
      }
      
      # isolate recipients of requests, check for surplus
      V(g)$surplus <- 0  # default surplus
      V(g)[!(is.na(V(g)$imp))]$surplus <- 
        V(g)[!(is.na(V(g)$imp))]$stock - minstock
      
      # consider requests, transfer accordingly
      for(i in V(g)[!(is.na(V(g)$imp))]){
        if(V(g)[i]$stock > minstock){    # precaution measure; can give?
          if(V(g)[i]$surplus > V(g)[V(g)[i]$imp]$req){
            # surplus > requested amount
            V(g)[V(g)[i]$imp]$stock <- 
              V(g)[V(g)[i]$imp]$stock + V(g)[V(g)[i]$imp]$req
            
            V(g)[i]$stock <- 
              V(g)[i]$stock - V(g)[V(g)[i]$imp]$req
          }else{
            # surplus = or < requested amount
            V(g)[V(g)[i]$imp]$stock <- 
              V(g)[V(g)[i]$imp]$stock + V(g)[i]$surplus
            
            V(g)[i]$stock <- 
              V(g)[i]$stock - V(g)[i]$surplus
          }
          #V(g)[V(g)[i]$imp]$req
          #V(g)[i]$surplus
        }
      }
      
    }  # request process (osotua) active if applicable
    
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
    dtr[t,4] <- V(g)$req[1]; dtr[t,5] <- V(g)$req[2]
    
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


# per simulation analysis
#dt <- data.frame(dtr)
#colnames(dt) <- c('t','p1','p2')
#ggplot(dt, aes(x=t, y=p1)) + geom_line() +
# geom_line(aes(x=t, y=p2), colour='blue')

