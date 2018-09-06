getAisleTimes <- function(data, FootPosition, time, gg,
                          aisles, full.images, save.data, i){
  
  if(full.images){
    gg.aisles <- gg
    
    # Create rectangles of aisles. append and $layers is used to plot
    # the rectangles and names UNDER the path
    suppressWarnings(
      gg.aisles$layers <- append(gg.aisles$layers,
                                 geom_rect(data = aisles,
                                           mapping = aes(xmin = xmin, xmax = xmax,
                                                         ymin = zmin, ymax = zmax,
                                                         colour = colour, x = x, y = y), 
                                           fill = 'red', alpha = .3),
                                 after = 1)
    )
    
    gg.aisles$layers <- append(gg.aisles$layers,
                               geom_label(data = aisles, 
                                          mapping = aes(x = xmin, y = zmin, 
                                                        label = aisles$aisle.names),
                                          label.size = .32),
                               after = 2)
  } else {
    gg.aisles <- gg
  }
  
  
  #Function to check whether a coordinate is in one of the aisles.
  CheckPathInAisle <- function(position, aisles){
    position[1] > aisles$xmin & position[1] < aisles$xmax &
      -position[2] > aisles$zmin & -position[2] < aisles$zmax
  }
  
  # a is a matrix with [n.aisles, n.time points] with T or F.
  # T indicating that timepoint was spent in that aisle
  a <- apply(FootPosition[, c(1, 3)], 1, CheckPathInAisle, aisles = aisles)
  # t is a matrix with 2 columns. [, 1] = aisle number and [, 2] = time point
  t <- which(a, arr.ind = TRUE)
  aisle.time.points <- t[, 2]
  
  if(any(a) &  save.data){
    time.in.aisle <- time[t[c(diff(t[,2]) > 1, T), 2]] - time[t[c(T, diff(t[,2]) > 1), 2]] #time spent in each aisle (which aisle is speficied later)
    times.through.aisle <- t[c(T, diff(t[,2]) > 1), ]
    times.through.aisle <- data.frame(aisles$aisle.names[times.through.aisle[, 1]], times.through.aisle, time.in.aisle)
    colnames(times.through.aisle)[1:3] <- c("aisle.names", "aisle", "Entry.time.point")
    times.through.aisle$aisle.names <- factor(times.through.aisle$aisle.names, levels = aisles$aisle.names)
    #time.through.aisle is now a data frame with aisle names (as a factor to include all aisle),
    #aisle, numeric, number corresponding to aisle name
    #entry time point, when someone entered that aisle
    #time.in.aisle, time spent in each aisle
    
    
    data[i, 4:14] <- table(times.through.aisle$aisle.names) #Count number of times in each aisle
    #Calculate total time in each aisle
    aisle.times <- tapply(times.through.aisle$time.in.aisle, 
                          list(Category = times.through.aisle$aisle.names), 
                          FUN = sum)
    aisle.times[is.na(aisle.times)] <- 0
    data[i, 15:25] <- aisle.times
    
    
  } else {
    data[i, 4:25] <- 0
  }
  
  res.aisles <- list(gg.aisles = gg.aisles,
                     data = data,
                     aisle.time.points = aisle.time.points)
  return(res.aisles)
}



