checkTO2.serial <- function(traj1,traj2){
  
  id.list1 <- unique(mt_track_id(traj1))
  id.list2 <- unique(mt_track_id(traj2))
  pairs <- expand.grid(id.list1,id.list2,stringsAsFactors=F)
  
  
  #Get all the unique combinations between one group
  names(pairs) <- c('ID1','ID2')
  #pairs <- pairs[-which(pairs$ID1 == pairs$ID2),]
  pairs <- pairs[order(pairs$ID1),]
  pairs <- pairs[!duplicated(t(apply(pairs, 1, sort))),]   #Do we always want to get rid of duplicated pairs, i think so...
  n.pairs <- nrow(pairs)
  
  pairs$TO <- FALSE
  pairs$t.min <- NA
  pairs$t.max <- NA
  
  for (i in 1:n.pairs){
    traja <- traj1[mt_track_id(traj1)==pairs$ID1[i],]
    trajb <- traj2[mt_track_id(traj2)==pairs$ID2[i],]
    
    pp<-pairs[i,] #gets line of data to make with this iteration
    
    #Get nearest fixes that are within tc from one another.
    t1 <- mt_time(traja)
    t2 <- mt_time(trajb)
    
    #identify the temporal overlap period
    t.min <- max(c(min(t1),min(t2)))
    t.max <- min(c(max(t1),max(t2)))
    
    #check to see if there is an overlap period
    if (t.min > t.max){
      TO <- FALSE
      t.min <- t.max <- NA
    } else {
      TO <- TRUE
    }
    
    #makes data
    pp$TO <- TO
    pp$t.min <- t.min
    pp$t.max <- t.max
    
    pairs$TO[i] <- TO
    pairs$t.min[i] <- t.min
    pairs$t.max[i] <- t.max
    
  }
  pairs$t.min <- as.POSIXct(pairs$t.min)
  pairs$t.max <- as.POSIXct(pairs$t.max)
  return(pairs)
}

