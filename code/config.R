# VR Supermarkt: Set configuration parameters for feature extraction
#
# Last edited 2018-07-02 by Sjoerd
params <- list(
  
  # name of image of supermarket
  img.name = 'Supermarkt2.png',
  
  # directory with input files
  input.dir = "test",
  
  # directory with output files
  output.dir = "test2",
  
  # what output is saved (TRUE/FALSE)
  # It is adviced that full.images and save.data are both either TRUE or FALSE.
  # This ensures the features and the images shown always correspond.
  raw.images = TRUE,  # Images with just the path
  full.images = TRUE, # Images with path + aisles, stops, crosses, etc.
  save.data = FALSE,   # should stops, slows etc. be saved to an excel file
  make.gif = FALSE,   # Gif of a path DOES NOT WORK RIGHT NOW
  
  features = list(

    # Stops measures the amount of stops. A stop is defined as less than 
    # stop.radius movement in at least stop.time from a point
    stops = list(
      stop.time = 2,      # seconds of stopping
      stop.radius = 0.25  # meters of walking
    ),
    
    # Slows measures the times someone has slowed. Is defined the same as a
    # stop, but with different numbers
    slows = list(
      slow.time = 4,   # seconds of stopping
      slow.radius = 1    # meters of walking
    ),
    
    # Crossings
    cross = list(
      cross.lag1 = 3, #Time required for a path to cross itself before it counts as a crossing
      cross.lag2 = 3  #Minimum time required between two crossings
    ),
    
    # Aisles, both names and coordinates are defined here
    aisles = data.frame(xmin = c(rep(c(5, 9.2, 13, 17, 21), 2), 25.2),
                        xmax = c(rep(c(8.5, 12.3, 16.3, 20.3, 24.5), 2), 29.3),
                        zmin = c(rep(-37.5, 5), rep(-24.9, 6)),
                        zmax = c(rep(-30.3, 5), rep(-13.9, 6)), 
                        aisle.names = c(paste0(rep(1:5, 2), rep(LETTERS[1:2], each = 5)), "6B"), # Names of aisles
                        x = 1, y = 1, colour = 1 # Required for gg plot to work (don't ask why)
                        ) 
    
    
  )
)
