
## Image Partition

image_partition = function(image_no, data, show.plot = FALSE, density_window = 300, threshold = 0.01){
  
  ## Obtaining selected image
  image = data %>% filter(imageid == unique(data$imageid)[image_no])
  
  
  ## Converting to ppp object
  image_ppp = ppp(image$x, image$y,
                  window = owin(xrange = range(image$x),
                                yrange = range(image$y))
  )
  
  
  ## Obtaining image density
  image_density = spatstat::density.ppp(image_ppp, sigma = density_window)
  
  ## Obtaining density ratio (against mean)
  image_density_ratio <- image_density/mean(image_density)
  
  ## Matrix of points with ratio > 0.01
  m <- as.matrix(image_density_ratio > threshold) * 1
  
  ## Assigning unique labels to groups of cells that bunch together
  b <- bwlabel(m)
  b[b == 0] <- NA
  
  ## Replacing the density values with these groups of cells
  image_density_ratio$v <- b
  
  ## Converting original x and y coords to nearest pixel from the den_ratio
  np <- spatstat::nearest.valid.pixel(image_ppp$x, image_ppp$y, image_density_ratio)
  
  ## Getting only those groups of cells that are represented in data
  w <- image_density_ratio$v[cbind(np$row, np$col)]
  
  
  
  ## If specify show.plot = TRUE generate and display plot
  if(show.plot == TRUE) {
    
    ## Adding information to data about which "group" the cells belong to
    marked_ppp <- image_ppp
    marks(marked_ppp) <- as.character(w)
    
    ## Plotting
    cluster_plot = ggplot(data.frame(marked_ppp)) +
      aes(x = x, y = y, colour = marks) +
      geom_point(size = 0.5) +
      labs(x = "X", y = "Y", colour = "Cluster", title = "Cluster Plot") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5))
    
    
    ## Obtaining convex hulls around these regions to use as windows
    # reg <- NULL
    # for(i in unique(marks(marked_ppp))){
    #   Y <- marked_ppp[marks(marked_ppp) == i, ]
    #   reg[[i]] <- convexhull(Y)
    # }
    
    
    ## Returning objects
    return_list = list(
      cluster_plot = cluster_plot,
      cluster = as.character(w)
    )
    
    return(return_list)
    
    
    ## If show.plot = FALSE (default), do not generate or show plot and only print cluster
  } else {
    
    cluster = as.character(w)
    
    return(cluster)
  }
  
  
}
