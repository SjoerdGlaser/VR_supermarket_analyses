getSlows <- function(data, FootPosition, time, gg,
                     stop.points, slow.time, slow.radius,
                     full.images, save.data, i){
  
  final.step <- first(which(time > max(time) - slow.time)) - 1
  slow <- data.frame(begin.slow = rep(FALSE, length(time)), end.slow = numeric(length(time)))
  
  k <- dis <- final.step.in.slow <- 0
  for(b in 1 : final.step){
    if(b %% 1000 == 0){
      print(paste0('Calculating SLOW of time point ', b, ' of ', final.step, 
                   ' of file ', i))  }  
    
    if(final.step.in.slow > b || b %in% stop.points){ #Don't check points that are already part of previous slow or a stop
      next
    }
    
    dis <- 0
    k <- b
    while(dis < slow.radius && !k %in% stop.points && k < length(time)){
      k <- k + 1
      dis <- sqrt((FootPosition$x[b] - FootPosition$x[k]) ^ 2 +
                    (FootPosition$z[b] - FootPosition$z[k]) ^ 2  
      )
    }
    
    if(time[k - 1] - time[b] > slow.time) {
      slow[b, 1] <- TRUE
      slow[b, 2] <- final.step.in.slow <- k
    }
    
  }
  
  res <- list()
  
  n.slows <- length(which(slow[, 1]))
  data$n.slows[i] <- n.slows
  
  gg.slows <- gg
  
  if(full.images){
    if(any(slow[, 1])){
      for(s in which(slow[, 1])){
        slows.df <- FootPosition[c(s : slow[s, 2]), ]
        gg.slows$layers <- append(gg.slows$layers,
                                  geom_encircle(data = slows.df,
                                                mapping = aes(x = x, y = -z) , s_shape = .5, 
                                                expand = .015, fill = 'white'),
                                  after = 2)
      }
      gg.slows <- gg.slows + geom_text(aes(y = -48, x = 3, 
                                           label = paste("N slows = ", n.slows)),
                                       colour = 'white', size = 5)
    } else {
      gg.slows <- gg.slows + geom_text(aes(y = -48, x = 3, 
                                           label = paste("N slows = ", 0)),
                                       colour = 'white', size = 5)
    }
    
    
    gg.slows <- gg.slows +
      theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),legend.position="none",
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())
  }
  
  res <- list(gg.slows = gg.slows)
  
  if(save.data){
    res <- c(res,
             data = list(data),
             n.slows = n.slows)
  }
  
  return(res)
}