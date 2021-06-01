
## cellprop_region function

cellprop_region = function(data, imageID = FALSE) {
  
  
  ## Original format
  
  cellprop = data %>% 
    
    ## Obtaining counts of cell types by region, response, description
    group_by(region, imageID_new, response, description) %>% 
    summarise(count = n()) %>% 
    
    ## Obtaining proportions
    mutate(proportion = count/sum(count)) %>% 
    ungroup()
  
  
  
  ## Wide format
  
  cellprop_wide = cellprop %>% 
    select(-count) %>% 
    tidyr::pivot_wider(names_from = "description", 
                       values_from = "proportion",
                       
                       ## Replacing NAs with 0s (they should be 0s)
                       values_fill = 0)
  
  
  ## Converting response to factor
  cellprop_wide$response = factor(cellprop_wide$response)
  
  return(cellprop_wide)
}