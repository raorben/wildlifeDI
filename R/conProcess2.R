conProcess2 <- function(traj,traj2,dc=0,tc=0,GetSim=TRUE,return='move2'){
  
  #global variables in group_by hack
  id1 <- NULL
  dist <- NULL
  row1 <- NULL
  
  #Unit control
  units(tc) <- as_units("s")
  
  if (missing(traj2)){
    pairs <- checkTO(traj)
    pairs <- pairs[pairs$TO==TRUE,]
    mtraj <- traj
  } else {
    pairs <- checkTO2(traj,traj2)
    pairs <- pairs[pairs$TO==TRUE,]
    if (st_crs(traj2) != st_crs(traj)){
      traj2 <- st_transform(traj2,crs=st_crs(traj))
    }
    mtraj <- data.frame(id = c(mt_track_id(traj),mt_track_id(traj2)),
                        time = c(mt_time(traj),mt_time(traj2)),
                        geometry = c(traj[[attr(traj,'sf_column')]],traj2[[attr(traj2,'sf_column')]])) |>
      st_as_sf(sf_column_name = "geometry", crs=st_crs(traj)) |>
      mt_as_move2(time_column='time',track_id_column='id')
  }
  
  n.pairs <- nrow(pairs)
  condf <- NULL
  for (i in 1:n.pairs){
    traja <- mtraj[mt_track_id(mtraj)==pairs$ID1[i],]
    trajb <- mtraj[mt_track_id(mtraj)==pairs$ID2[i],]
    
    if (GetSim){
      trajs <- GetSimultaneous2(traja,trajb,tc)
      trajs1<-trajs[[1]]
      trajs2<-trajs[[2]]
      tr1 <- trajs1[mt_track_id(trajs1)==pairs$ID1[i],]
      tr2 <- trajs2[mt_track_id(trajs2)==pairs$ID2[i],]
      
      if(nrow(tr1)==0){next}
      if(nrow(tr2)==0){next}
      proxdf <- data.frame(id1=pairs$ID1[i],id2=pairs$ID2[i],
                           row1=row.names(tr1),row2=row.names(tr2),
                           dist=st_distance(tr1,tr2,by_element=TRUE),
                           difftime=as.numeric(abs(mt_time(tr1)-mt_time(tr2))))
      #Unit control
      units(proxdf$difftime) <- as_units('s')
      units(dc) <- units(proxdf$dist)
      proxdf <- proxdf[ proxdf$dist < dc & proxdf$difftime < tc, ]
      
    } else { #If we don' use GetSim We can have multiple contacts from different sources.
      dM <- st_distance(traja,trajb)
      tM <- abs(-outer(as.numeric(mt_time(traja)),as.numeric(mt_time(trajb)),'-'))
      
      units(tM) <- as_units('s')
      units(dc) <- units(dM)
      
      rownames(tM) <- rownames(traja)
      colnames(tM) <- rownames(trajb)
      
      ind <- which(dM < dc & tM < tc, arr.ind=TRUE)
      
      if (length(ind) > 0){
        rnm1 <- rownames(tM)[which(dM < dc & tM < tc, arr.ind = TRUE)[, 1]]
        rnm2 <- colnames(tM)[which(dM < dc & tM < tc, arr.ind = TRUE)[, 2]]
        proxdf <- data.frame(id1=pairs$ID1[i],id2=pairs$ID2[i],row1=rnm1,row2=rnm2,dist=dM[ind],difftime=tM[ind])
      } else {
        proxdf <- NULL
      }
      
    }
    if (is.null(condf)) {condf <- proxdf} else {condf <- rbind(condf,proxdf)}
  }
  
  
  
  #arrange return object
  if (return=='contact'){
    return(condf)
  } else {
    #Create contact list for both pair directions in case of 1 group
    if (missing(traj2)){
      dfr <- rbind(condf, 
                   data.frame(id1=condf$id2,id2=condf$id1,row1=condf$row2,row2=condf$row1,dist=condf$dist,difftime=condf$difftime))
    } else {
      dfr <- condf
    }
    
    dfr.d <- dfr |>
      dplyr::group_by(id1,row1) |>
      dplyr::slice_min(dist)
    
    dfr.n <- dfr |>
      dplyr::group_by(id1,row1) |>
      dplyr::summarise(ncon=dplyr::n(),
                       .groups='drop')
    
    traj$contact <- 0
    traj$contact_id <- NA
    traj$contact_d <- NA
    traj$contact_dt <- NA
    traj$contact_n <- NA
    
    ind.con <- match(dfr.d$row1, row.names(traj))
    
    traj$contact[ind.con] <- 1
    traj$contact_id[ind.con] <- dfr.d$id2
    traj$contact_d[ind.con] <- dfr.d$dist
    traj$contact_dt[ind.con] <- dfr.d$difftime
    traj$contact_n[ind.con] <- dfr.n$ncon
    
    return(traj)
  }
  
}  
