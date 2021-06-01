
## Obtaining t-sne plots for image

get_tsne = function(treatment, combdata, monodata){
  
  ## Defining data to use for the rest of the function
  if(treatment == "combination"){
    dat = combdata
  } else if(treatment == "monotherapy"){
    dat = monodata
  }
  
  
  
  # Obtaining data in matrix form
  
  
  dat_mat = 
    dat %>% 
    dplyr::select(-imageID, -response, -batch) %>% 
    as.matrix() %>% 
    
    # ## Sqrt transformation
    # apply(2, sqrt)
    
    ## Changing column names
    `rownames<-`(dat$imageID)
  
  
  
  # Performing t-sne analysis for 2, 5, and 10 perplexities
  
  
  ## Setting random seed for reproducibility
  set.seed(2021)
  
  ## Initialising list to store plots
  dat_tsne_list = list()
  
  ## For loop for different perplexity values
  for(perplexity in c(2, 5, 10)){
    
    ## Performing t-SNE
    dat_tsne = dat_mat %>% 
      Rtsne(perplexity = perplexity)
    
    ## Creating data for plotting
    dat_tsne_plotdata = tibble(
      `Dimension 1` = dat_tsne$Y[, 1],
      `Dimension 2` = dat_tsne$Y[, 2],
      `Response` = dat$response
    )
    
    
    
    # Obtaining t-sne plots
    
    
    
    ## Obtaining plots for different perplexities
    dat_tsne_list[[match(perplexity, c(2, 5, 10))]] = dat_tsne_plotdata %>% 
      ggplot() +
      aes(x = `Dimension 1`, y = `Dimension 2`, colour = `Response`) +
      geom_point() +
      labs(title = paste(str_to_title(treatment), "Therapy t-SNE"),
           subtitle = paste("Perplexity =", perplexity)) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5))
    
  }
  
  return(
    gridExtra::grid.arrange(
      dat_tsne_list[[1]],
      dat_tsne_list[[2]],
      dat_tsne_list[[3]],
      ncol = 2)
  )
}