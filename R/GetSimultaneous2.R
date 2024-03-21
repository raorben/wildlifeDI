GetSimultaneous2 <- function(traj1,traj2,tc=0){
  
  #Set units of tc to seconds
  units(tc) <- as_units('s')
  #get the length of each trajectory
  n1 <- nrow(traj1)
  n2 <- nrow(traj2)
  
  #Get nearest fixes that are within tc from one another.
  match1 <- data.frame()
  t1 <- mt_time(traj1)
  t2 <- mt_time(traj2)
  
  for (i in 1:n1){
    matched <- which.min(abs(difftime(t1[i],t2,units="secs")))
    temp <- data.frame(tr1=i,tr2=matched,dt=as.numeric(abs(difftime(t1[i],t2[matched],units="secs")))  )
    match1 <- rbind(match1,temp)
  }
  
  match2 <- data.frame()
  for (i in 1:n2){
    matched <- which.min(abs(difftime(t2[i],t1,units="secs")))
    temp <- data.frame(tr1=matched,tr2=i,dt=as.numeric(abs(difftime(t2[i],t1[matched],units="secs")))  )
    match2 <- rbind(match2,temp)
  }
  
  units(match1$dt) <- as_units("s")
  units(match2$dt) <- as_units("s")
  match1 <- match1[which(match1$dt <= tc),]
  match2 <- match2[which(match2$dt <= tc),]
  
  
  # This filtering check is still not working exactly how I want it to. 
  # Basically if tc is set to half the fix interval fixes exactly on that interval are lost if you run GetSimultaneous 2x on the data. 
  ind <- NULL
  for (i in unique(match1$tr2)){
    ind1 <- which(match1$tr2 == i)
    ind1 <- ind1[which.min(match1$dt[ind1])]
    ind <- c(ind,ind1)
  }
  match1 <- match1[ind,]
  ind <- NULL
  for (i in unique(match2$tr1)){
    ind1 <- which(match2$tr1 == i)
    ind1 <- ind1[which.min(match2$dt[ind1])]
    ind <- c(ind,ind1)
  }
  
  match2 <- match2[ind,]
  
  ind.1 <- which(is.na(match(match1$tr1,match2$tr1))==TRUE)
  if (length(ind.1) > 0){ match1 <- match1[-ind.1,] }
  ind.2 <- which(is.na(match(match2$tr1,match1$tr1))==TRUE)
  if (length(ind.1) > 0){ match2 <- match2[-ind.2,] }
  
  tr1.sim <- traj1[match1$tr1,]
  tr2.sim <- traj2[match1$tr2,]
  
  #Return the two move2 objects.
  trj<-list(tr1.sim,tr2.sim)
  return(trj)
}
