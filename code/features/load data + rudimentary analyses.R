createDataFrame <- function(data.files){
  data <- data.frame(name = character(length(data.files)),
                     total.time = numeric(length(data.files)),
                     total.distance = numeric(length(data.files)),
                     n.aisle.1A = numeric(length(data.files)),
                     n.aisle.2A = numeric(length(data.files)),
                     n.aisle.3A = numeric(length(data.files)),
                     n.aisle.4A = numeric(length(data.files)),
                     n.aisle.5A = numeric(length(data.files)),
                     n.aisle.1B = numeric(length(data.files)),
                     n.aisle.2B = numeric(length(data.files)),
                     n.aisle.3B = numeric(length(data.files)),
                     n.aisle.4B = numeric(length(data.files)),
                     n.aisle.5B = numeric(length(data.files)),
                     n.aisle.6B = numeric(length(data.files)),
                     time.aisle.1A = numeric(length(data.files)),
                     time.aisle.2A = numeric(length(data.files)),
                     time.aisle.3A = numeric(length(data.files)),
                     time.aisle.4A = numeric(length(data.files)),
                     time.aisle.5A = numeric(length(data.files)),
                     time.aisle.1B = numeric(length(data.files)),
                     time.aisle.2B = numeric(length(data.files)),
                     time.aisle.3B = numeric(length(data.files)),
                     time.aisle.4B = numeric(length(data.files)),
                     time.aisle.5B = numeric(length(data.files)),
                     time.aisle.6B = numeric(length(data.files)),
                     n.crossings = numeric(length(data.files)),
                     n.stops = numeric(length(data.files)),
                     n.slows = numeric(length(data.files)),
                     n.datapoints = numeric(length(data.files)),
                     datapoints.per.second = numeric(length(data.files)),
                     stringsAsFactors = FALSE
  )
  return(data)
}

runFirstAnalyses <- function(JSONfile, image,
                             raw.images, save.data, data,
                             input.dir, output.dir, i){
  # Read the JSON files, remove excess data, 
  # save first basic results (total time, total distance and n data points),
  # make first gg plot and save it
  #
  # JSONfile   = name of the JSON file required
  # image      = the image for the background
  # raw.images = TRUE/FALSE should the raw image be saved
  # save.data  = TRUE/FALSE should the data be saved to a CSV file
  # data       = data frame to save the data in the end to a CSV file
  # input.dir  = directory of the input JSON files
  # output.dir = directory of the output image files
  # i          = number of data file, required to save to in data
  
  # Read data
  suppressWarnings(
    dat <- fromJSON(readLines(paste0('input/', input.dir, '/', JSONfile)),
                    simplifyDataFrame = TRUE)
  )

  # Remove duplicate data (speeds up all analyses)
  time <- dat[[1]]$m_PupilTime
  dup <- which(diff(time) == 0)
  FootPosition <- dat[[1]]$m_FootPosition
  if(length(dup) > 0){
    time <- time[-dup]
    FootPosition <- FootPosition[-dup, ]
    row.names(FootPosition) <- 1:nrow(FootPosition)
  }
  
  # Remove all datapoints before start of the task
  first <- first(which(FootPosition$z < 45.5 & FootPosition$z >10))
  if(first > 1){
    FootPosition <- FootPosition[-1 : -first, ]
    time <- time[-1 : -first]
  }
  
  # Remove all datapoints after end of the task
  last <- last(which(FootPosition$z < 45.5))
  if(last < nrow(FootPosition)){
    FootPosition <- FootPosition[-last : -nrow(FootPosition), ]
    time <- time[-last : -length(time)]
  }
  row.names(FootPosition) <- 1:nrow(FootPosition)
  
  # Save results of basic analyses
  if(save.data){
    data$name[i] <- JSONfile
    data$total.time[i] <- last(time) - time[1]
    x.change <- diff(FootPosition$x, 1)
    y.change <- diff(FootPosition$z, 1)
    data$total.distance[i] <- sum(sqrt(x.change ^ 2 + y.change ^ 2))
    data$n.datapoints[i] <- length(time)
    data$datapoints.per.second[i] <- length(time) / (last(time) - time[1])
    
  }
  
  # Make and save plot of raw walking path
  gg <- ggplot() + 
    #ylim(-53, -7) + xlim(0, 29) +
    scale_x_continuous(limits = c(0, 30), expand=c(0,0)) +
    scale_y_continuous(limits = c(-50, -2.5), expand=c(0,0)) +
    annotation_custom(rasterGrob(image, 
                                 width = unit(1, "npc"), 
                                 height = unit(1, "npc")), 
                      -Inf, Inf, -Inf, Inf) +
    geom_path(data = FootPosition, 
              mapping = aes(x = x, y = -z, color = 1:length(x)),
              arrow = arrow(length = unit(5, "points"))) + 
    #    coord_fixed() + 
    coord_flip() +
    theme(legend.position = "none",
          plot.margin = margin(0, 0, 0, 0, "cm")) +
    geom_text(aes(y = -12, x = 2, 
                  label = paste("Person", 
                                substr(JSONfile[i], 1, 5),
                                "\n Walking route")), 
              size = 5) + 
    geom_hline(yintercept = -45.5)
  
  gg
  
  # Save if required
  if(raw.images){
    # Create path if exists and create if it does not
    if( ! file.exists(paste0('output/png/', output.dir))){
      dir.create(paste0('output/png/', output.dir))
    }
    ggsave(paste0('output/png/', output.dir, '/',
                  JSONfile, '_RAW.png'), gg, 
           width = 37.5, height = 21, units = 'cm')
  }
  
  res <- list(dat = dat,
              gg = gg, 
              FootPosition = FootPosition,
              time = time, 
              data = data)
  
  return(res)
}