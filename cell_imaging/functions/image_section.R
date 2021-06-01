
## image_section function

image_section = function(image_no, data, 
                         margin_width = 100, 
                         density_threshold = 0.99, 
                         density_sigma = 50, 
                         core_use_density = TRUE,
                         core_dist_threshold = 0.90){
  
  ## Obtaining selected image
  image = data %>% filter(imageID == unique(data$imageID)[image_no])
  
  
  ## Converting to ppp object
  image_ppp = ppp(image$x, image$y,
                  window = owin(xrange = range(image$x),
                                yrange = range(image$y))
  )
  
  ## Adding marks
  marks(image_ppp) = image %>% select(-x, -y)
  
  ## Obtaining melanoma cells only
  image_mel = image %>% filter(label == "tumour")
  image_mel_ppp = ppp(image_mel$x, image_mel$y,
                      window = owin(xrange = range(image_mel$x),
                                    yrange = range(image_mel$y)))
  marks(image_mel_ppp) = image_mel %>% select(-x, -y)
  
  ## Obtain concave window of melanoma cells
  concave_window = suppressMessages(
    makeWindowSmall(window = "concave", window.length = 2,
                    data = image_mel_ppp) # makeWindowSmall function sourced in rmd
  )
  
  
  
  # Core cells
  
  
  
  ## Density method
  
  if(core_use_density == TRUE){
    
    ## Obtaining density of melanoma cells only
    melimage_density = density(image_mel_ppp, sigma = density_sigma)
    
    ## Obtain core cell indexes by obtaining density above specified density_threshold
    core_v = as.matrix(melimage_density$v > quantile(melimage_density$v, density_threshold))*1
    
    ## Obtaining the largest cluster to assign as core
    core_v_b = bwlabel(core_v)
    core_v_b[core_v_b == 0] = NA
    largest_core = table(core_v_b) %>% sort(decreasing = TRUE) %>% .[1] %>% names()
    # core_v_b[core_v_b != largest_core] = NA
    
    ## Replacing melimage_density density values with only those above threshold
    core_density = melimage_density
    core_density$v = core_v_b
    
    ## Converting original x and y coords to nearest pixel from the image density
    np = spatstat.geom::nearest.valid.pixel(image_ppp$x, image_ppp$y, core_density)
    w = core_density$v[cbind(np$row, np$col)]
    
    ## Getting only those cells with non-NA x and y coordinates (i.e. above threshold)
    core_index = which(!is.na(w))
    core_cells = data.frame(image_ppp[core_index, ]) %>% 
      ### Adding region as "core"
      mutate(region = "core")
    
  } else if(core_use_density == FALSE) {
    
    
    ## Distance method
    
    
    ## Obtaining new ppp with melama point in concave window
    image_mel_concave = ppp(image_mel$x, image_mel$y,
                            marks = marks(image_mel_ppp),
                            window = concave_window)
    
    ## Obtaining distance to border
    border_dist = bdist.points(image_mel_concave)
    
    ## Filtering for only those that are further than core_dist_threshold quantile of distances
    core_index = (border_dist > quantile(border_dist, core_dist_threshold))
    core_cells = data.frame(image_mel_concave[core_index, ]) %>% 
      ### Adding region as "core"
      mutate(region = "core")
    
    
  }
  
  
  
  
  # Margin cells
  
  
  
  ## Obtain margin by specified margin width
  margin_window = spatstat.geom::border(concave_window, r = margin_width)
  
  ## Obtaining cells in the margin (inside margin_window)
  margin_index = inside.owin(image_ppp, w = margin_window)
  margin_cells = data.frame(image_ppp[margin_index]) %>%
    ### Adding region as "margin"
    mutate(region = "margin")
  
  
  
  # Non-melanoma cells
  
  
  
  ## Obtaining cells outside concave window (NOT inside concave_window)
  nonmel_cells = image %>% filter(label == "stroma") %>% 
    ### Adding region as "nonmelanoma"
    mutate(region = "nonmelanoma")
  
  
  ## Selectively update region data in original dataframe
  data_tojoin = rbind(core_cells, margin_cells, nonmel_cells) %>% select(x, y, region) %>% data.table::data.table()
  image[data_tojoin, on = c(x = "x", y = "y"), region := i.region ]
  
  
  
  ## Returning completed image no and updated dataframe
  message(paste("Completed image", image_no))
  return(image)
}
