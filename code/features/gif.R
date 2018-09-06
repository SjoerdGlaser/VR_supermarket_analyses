# plot
makeGifOfPath <- function(FootPosition, time, JSONfile, img){
  
  time1 <- 6 * round(time/6)
  
  ggani <- ggplot(FootPosition, 
                  aes(x = x, y = -z, color = 1:length(x), 
                      frame = time1, cumulative = TRUE)) + 
    ylim(-53, -7) + xlim(0, 29) +
    annotation_custom(rasterGrob(img, 
                                 width = unit(1, "npc"), 
                                 height = unit(1, "npc")), 
                      -Inf, Inf, -Inf, Inf) +
    geom_path(arrow = arrow(length = unit(5, "points"))) + 
    coord_equal(1) + 
    coord_flip() +
    theme(legend.position = "none") +
    geom_text(aes(y = -12, x = 2, 
                  label = paste("Person", 
                                substr(JSONfile, 1, 5),
                                "\n Walking route"))) + 
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
  
  if( ! file.exists(paste0('output/gif/', params$output.dir))){
    dir.create(paste0('output/gif/', params$output.dir))
  }
  
  JSONfile <- substr(JSONfile, 1, 21)
  gganimate(ggani, interval = .15, ani.width = 4429, ani.heigth = 2480,
            paste0('output/gif/', params$output.dir, '/', JSONfile, '.gif'))
}