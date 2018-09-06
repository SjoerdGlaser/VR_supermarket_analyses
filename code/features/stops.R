getStops <- function(data, FootPosition, time, gg,
                     stop.time, stop.radius,
                     save.data, full.images, i){
  
  final.step <- first(which(time > max(time) - stop.time)) - 1
  
  stop <- data.frame(begin.stop = rep(FALSE, length(time)), end.stop = numeric(length(time)))
  
  k <- dis <- final.step.in.stop <- 0
  for(s in 1 : final.step){
    if(s %% 1000 == 0){
      print(paste0('Calculating STOP of time point ', s, ' of ', final.step, 
                   ' of file ', i))
    }
    
    if(final.step.in.stop > s){ #Don't check points that are already part of previous stop
      next
    }
    
    dis <- 0
    k <- s
    while(dis < stop.radius && k < length(time)){
      k <- k + 1
      dis <- sqrt((FootPosition$x[s] - FootPosition$x[k]) ^ 2 +
                    (FootPosition$z[s] - FootPosition$z[k]) ^ 2  
      )
    }
    
    if(time[k - 1] - time[s] > stop.time){
      stop[s : (k - 1), 1] <- TRUE
      stop[s, 2] <- final.step.in.stop <- k - 1
    }
  }
  
  stop.points <- which(stop[, 1])
  
  stop.start.points <- which(stop[, 2] != 0)
  n.stops <- length(stop.start.points)
  stop.pos <- FootPosition[stop.start.points, ]
  
  data$n.stops[i] <- n.stops
  
  res <- list()
  
  
  gg.stops <- gg
  
  if(full.images){
    if(n.stops > 0){
      gg.stops <- gg.stops +
        geom_circle(data = stop.pos, 
                    mapping = aes(x0 = x, y0 = -z, 
                                  r = stop.radius), 
                    fill = 'green', colour = 'green')
    }
    
    gg.stops <- gg.stops +
      geom_text(aes(y = -48, x = 2, 
                    label = paste("N stops = ", n.stops)),
                size = 5,
                colour = 'green')
  }
  
  res.stops <- list(gg.stops = gg.stops,
                    stop.points = list(stop.points),
                    data = list(data),
                    n.stops = n.stops)
  
  return(res.stops)
}