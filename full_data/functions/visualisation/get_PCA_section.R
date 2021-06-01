
## Obtaining PCA plots for image sections

get_PCA_section = function(section, treatment, combdata, monodata){
  
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
  
  
  
  # Performing PCA
  
  
  comb_pca = prcomp(comb_sectionprop_mat, scale = TRUE)
  
  ## Data for plotting
  comb_pca_df = tibble(
    PC1 = comb_pca$x[, 1],
    PC2 = comb_pca$x[, 2],
    PC3 = comb_pca$x[, 3],
    Response = region_dat$response
  )
  
  
  
  # Creating PCA plots
  
  
  comb_pca1 = comb_pca_df %>% 
    ggplot() +
    aes(x = PC1, y = PC2, colour = Response) +
    geom_point() +
    labs(title = paste(str_to_title(treatment), "PC1 ~ PC2:", section)) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  
  comb_pca2 = comb_pca_df %>% 
    ggplot() +
    aes(x = PC1, y = PC3, colour = Response) +
    geom_point() +
    labs(title = paste(str_to_title(treatment), "PC1 ~ PC3:", section)) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  
  comb_pca3 = comb_pca_df %>% 
    ggplot() +
    aes(x = PC2, y = PC3, colour = Response) +
    geom_point() +
    labs(title = paste(str_to_title(treatment), "PC2 ~ PC3:", section)) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  
  return(
    ggpubr::ggarrange(comb_pca1, comb_pca2, comb_pca3,
                      ncol = 3, common.legend = TRUE, legend = "bottom")
  )
}