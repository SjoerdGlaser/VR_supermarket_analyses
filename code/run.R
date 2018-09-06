# VR Supermarkt: Extract features
#
# Last edited 2018-06-25 by Sjoerd


# preamble ----------------------------------------------------------------
packages <- c("jsonlite", "tidyverse", "png", "ggforce",
              "ggalt", "Rcpp", "grid", "gganimate")

lapply(packages, require, character.only = TRUE)




source("code/config.R") # load the configuration file
sapply(list.files("code/features", full.names = TRUE, '.R'), source) # load FE funcs
sourceCpp('code/features/cppDoLinesIntersect.cpp') 


# Load image
image <- readPNG(paste0('input/', params$img.name))

# get data files
data.files <- list.files(
  path = file.path("input", params$input.dir),
  pattern = 'json',
  full.names = FALSE
)

# Create data frame to save results
data <- createDataFrame(data.files)

for(i in 1 : length(data.files)){
  JSONfile <- data.files[i]
  
  res <- runFirstAnalyses(JSONfile = JSONfile, image = image, 
                          raw.images = params$raw.images, 
                          save.data = params$save.data, 
                          data = data,
                          input.dir = params$input.dir, 
                          output.dir = params$output.dir, 
                          i = i)
  
  if(params$save.data | params$full.images){
    res.aisles <- getAisleTimes(data = res$data,
                                FootPosition = res$FootPosition,
                                time = res$time,
                                gg = res$gg,
                                aisles = params$features$aisles,
                                full.images = params$full.images,
                                save.data = params$save.data,
                                i = i)
    
    res.cross <- getCrossings(data = res.aisles$data,
                              FootPosition = res$FootPosition,
                              time = res$time,
                              gg = res.aisles$gg.aisles,
                              aisle.time.points = res.aisles$aisle.time.points,
                              aisles = params$features$aisles,
                              cross.lag1 = params$features$cross$cross.lag1,
                              cross.lag2 = params$features$cross$cross.lag2,
                              full.images = params$full.images,
                              i = i)
    
    res.stops <- getStops(data = res.cross$data,
                          FootPosition = res$FootPosition,
                          time = res$time,
                          gg = res.cross$gg.cross,
                          stop.time = params$features$stops$stop.time,
                          stop.radius = params$features$stops$stop.radius,
                          full.images = params$full.images,
                          save.data = params$save.data,
                          i = i)
    
    res.slows <- getSlows(data = res.stops$data, 
                          FootPosition = res$FootPosition, 
                          time = res$time, 
                          gg = res.stops$gg.stops,
                          stop.points = res.stops$stop.points, 
                          slow.time = params$features$slows$slow.time, 
                          slow.radius = params$features$slows$slow.radius,
                          full.images = params$full.images,
                          save.data = params$save.data, 
                          i = i)
    
    data <- res.slows$data
    
    if(params$full.images){
      if( ! file.exists(paste0('output/png/', params$output.dir))){
        dir.create(paste0('output/png/', params$output.dir))
      }
      # Save variables required for ggplot in global environment 
      # (ggplot cannot handle variables created in functions)
      aisles <- params$features$aisles
      n.crossings <- res.cross$n.crossings
      stop.radius <- params$features$stops$stop.radius
      n.stops <- res.stops$n.stops
      n.slows <- res.slows$n.slows
      
      JSONfile <- substr(JSONfile, 1, 21)
      ggsave(paste0('output/png/', params$output.dir, '/', JSONfile, '.png'), 
             res.slows$gg.slows, width = 37.5, height = 21, units = 'cm')
    }
  }
}

if(params$save.data){
  write.csv2(data, file = "output/csv/data.csv", row.names=FALSE)
}


if(params$make.gif){
  sapply(list.files("code/features", full.names = TRUE, '.R'), source) # load FE funcs
  
  JSONfile <- substr(JSONfile, 1, nchar(JSONfile) - 5)
  makeGifOfPath(FootPosition = res$FootPosition,
                time = res$time, JSONfile = JSONfile,
                img = image)
    
}
