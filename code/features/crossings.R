getCrossings = function(data, FootPosition, time, gg, 
                        aisle.time.points, aisles,
                        cross.lag1, cross.lag2,
                        full.images, i){
  
  print(paste('Calculating crossings of file', i))
  crossings <- anyIntersects(FootPosition$x, -FootPosition$z, time, cross.lag1)
  #A crossings consists of 4 time points (a, a+1, b, b+1), both a and b ares saved.
  #Only a is used to calculate n crossings and plot them.
  #b can be used in the future to calculate the angle.
  crossings <- t(unname(as.data.frame(crossings)))
  colnames(crossings) <- c("First Time point", "Second Time point")
  
  
  cross.points <- crossings[, 1]
  cross.points <- cross.points[!cross.points %in% aisle.time.points] 
  #Remove crossings in an aisle. A lot of performance could be won here by removing these before
  #calculating crossings. However, simply removing them will not do. Then a linesection will be
  #drawn from the entering an aisle to immediately after exiting.
  
  #### Old way if no missing data is present. You can just check if two positions are equal.
  #### This is kept because it is much quicker than the new way
  # for(j in 1 : (nrow(pos.r) - lag)){
  #    dup[j] <- any(pos.r[j, 1] == pos.r[(j + lag) : nrow(pos.r), 1] &
  #                    pos.r[j, 2] == pos.r[(j + lag) : nrow(pos.r), 2]
  #    )
  #  }
  #
  
  if(length(cross.points) > 1){ # If crossings exist
    cross.points <- cross.points[-1]
    cross.time <- time[cross.points]
    
    # Remove crossings which are cross.lag2 removed from a previous crossing
    l = 1 #Select first crossing
    if(length(cross.time) > 1){ #If there are at least 2 crossing
      d <- diff(cross.time)
      k = 0
      for(ii in 1:length(d)){
        if(ii > k){ # Don't look at crossings that have been skipped
          k = ii
          if(d[ii] < cross.lag2){
            dd = d[ii]
            while(dd < cross.lag2 & k < length(d)){ #Stop when there is more than lag between two crossings
              k = k + 1
              dd = dd + d[k]
            }
            if(dd < cross.lag2) # Stop if no crossings left
              break
          }
          
          l <- c(l, k + 1)
        }
      }
    }
    
    
    cross.points1 <- cross.points[l]
    n.crossings <- length(cross.points1)
    crossings.pos <- FootPosition[cross.points1, ]
    
    data$n.crossings[i] <- n.crossings
    gg.cross <- gg
    
    if(full.images){
      gg.cross <- gg +
        geom_point(data = crossings.pos,
                   aes(x = x, y = -z, size = 2),
                   col = 'blue') +
        geom_text(aes(y = -48, x = 4, 
                      label = paste("N crossings =", n.crossings)),
                  colour = 'blue')
    }
    
    #If no crossings are present
  } else {
    data$n.crossings[i] <- 0
    
    if(full.images){
      gg.cross <- gg +
        geom_text(aes(y = -48, x = 4, 
                      label = paste("N crossings = 0")),
                  colour = 'blue')
    }
  }
  res.cross <- list(gg.cross = gg.cross, 
                    data = data, n.crossings = n.crossings)
  
  return(res.cross)
}
