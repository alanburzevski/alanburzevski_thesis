
## Obtaining PCA plots for image

get_PCA = function(treatment, combdata, monodata){
  
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
  
  
  
  # Performing PCA
  
  
  dat_pca = prcomp(dat_mat, scale = TRUE)
  
  ## Data for plotting
  dat_pca_df = tibble(
    PC1 = dat_pca$x[, 1],
    PC2 = dat_pca$x[, 2],
    PC3 = dat_pca$x[, 3],
    Response = dat$response
  )
  
  
  
  # Creating PCA plots
  
  
  dat_pca1 = dat_pca_df %>% 
    ggplot() +
    aes(x = PC1, y = PC2, colour = Response) +
    geom_point() +
    labs(title = paste(str_to_title(treatment), "PC1 ~ PC2")) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  
  dat_pca2 = dat_pca_df %>% 
    ggplot() +
    aes(x = PC1, y = PC3, colour = Response) +
    geom_point() +
    labs(title = paste(str_to_title(treatment), "PC1 ~ PC3")) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  
  dat_pca3 = dat_pca_df %>% 
    ggplot() +
    aes(x = PC2, y = PC3, colour = Response) +
    geom_point() +
    labs(title = paste(str_to_title(treatment), "PC2 ~ PC3")) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  
  return(
    ggpubr::ggarrange(dat_pca1, dat_pca2, dat_pca3,
                      ncol = 3, common.legend = TRUE, legend = "bottom")
  )
}