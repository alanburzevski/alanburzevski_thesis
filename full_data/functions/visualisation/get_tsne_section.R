
## Obtaining t-sne plots for image sections

get_tsne_section = function(section, treatment, combdata, monodata){
  
  ## Defining region-specific data to use for the rest of the function
  if(treatment == "combination"){
    region_dat = combdata %>% filter(region == section)
  } else if(treatment == "monotherapy"){
    region_dat = monodata %>% filter(region == section)
  }
  
  
  
  # Obtaining data in matrix form
  
  
  comb_sectionprop_mat = 
    region_dat %>% 
    dplyr::select(-imageID, -response, -batch, -region) %>% 
    as.matrix() %>% 
    
    # ## Sqrt transformation
    # apply(2, sqrt)
    
    ## Changing column names
    `rownames<-`(region_dat$imageID)
  
  
  
  # Performing t-sne analysis for 2, 5, and 10 perplexities
  
  
  ## Setting random seed for reproducibility
  set.seed(2021)
  
  ## Initialising list to store plots
  comb_tsne_list = list()
  
  ## For loop for different perplexity values
  for(perplexity in c(2, 5, 10)){
    
    ## Performing t-SNE
    comb_tsne = comb_sectionprop_mat %>% 
      Rtsne(perplexity = perplexity)
    
    ## Creating data for plotting
    comb_tsne_plotdata = tibble(
      `Dimension 1` = comb_tsne$Y[, 1],
      `Dimension 2` = comb_tsne$Y[, 2],
      `Response` = region_dat$response
    )
    
    
    
    # Obtaining t-sne plots
    
    
    
    ## Obtaining plots for different perplexities
    comb_tsne_list[[match(perplexity, c(2, 5, 10))]] = comb_tsne_plotdata %>% 
      ggplot() +
      aes(x = `Dimension 1`, y = `Dimension 2`, colour = `Response`) +
      geom_point() +
      labs(title = paste(str_to_title(treatment), "Therapy t-SNE:", section),
           subtitle = paste("Perplexity =", perplexity)) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5))
    
  }
  
  return(
    gridExtra::grid.arrange(
      comb_tsne_list[[1]],
      comb_tsne_list[[2]],
      comb_tsne_list[[3]],
      ncol = 2)
  )
}