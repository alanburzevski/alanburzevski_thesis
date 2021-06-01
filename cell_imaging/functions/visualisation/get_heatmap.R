
## Obtaining heatmap for image

get_heatmap = function(treatment, combdata, monodata, allregion = FALSE){
  
  ## Defining data to use for the rest of the function
  if(treatment == "combination"){
    dat = combdata
  } else if(treatment == "monotherapy"){
    dat = monodata
  }
  
  
  
  # Obtaining data in matrix form
  
  
  dat_mat =
    dat %>% 
    select(-imageID, -response, -batch) %>% 
    as.matrix() %>% 
    t() %>% 
    
    # ## Sqrt transformation
    # apply(2, sqrt)
    
    ## Changing column names
    `colnames<-`(dat$imageID)
  
  
  
  # Obtaining annotations
  
  
  ## Column annotation: response and batch
  
  dat_anncol = data.frame("Response" = dat$response,
                          "Batch" = dat$batch) %>% 
    `rownames<-`(colnames(dat_mat)) # Setting rownames to imageid (colnames of barcodeprop_mat)
  
  
  ## Row annotation: melanoma
  
  if(allregion == FALSE){
    
    ## If no regiondata DO NOT have extra row annotation for region
    
    dat_annrow = data.frame("Cell Type" = rep("Non-melanoma", nrow(dat_mat))) # Populating with all Non-melanoma for now
      `colnames<-`("Cell Type") %>%  # For nicer plotting
      `rownames<-`(rownames(dat_mat))  # Setting rownames to cell types (rownames of cellprop_mat)
    
    ## Obtaining row indexes which are "melanoma" AND NOT "non-melanoma"
    melanoma_index = (str_detect(rownames(dat_annrow), "melanoma") & !str_detect(rownames(dat_annrow), "non-melanoma"))
    
    dat_annrow[melanoma_index, "Cell Type"] = "Melanoma" # Changing melanoma cells to melanoma classification
    
  } else if(allregion == TRUE){
    
    ## If regiondata INCLUDE extra row annotation for region
    
    dat_annrow = data.frame("Cell Type" = rep("Non-melanoma", nrow(dat_mat)), # Populating with all Non-melanoma for now
                            "Region" = rownames(dat_mat) %>% str_split("_") %>% sapply(function(x) x[1])) %>% # Obtaining region
      `colnames<-`(c("Cell Type", "Region")) %>%  # For nicer plotting
      `rownames<-`(rownames(dat_mat))  # Setting rownames to cell types (rownames of cellprop_mat)
    
    ## Obtaining row indexes which are "melanoma" AND NOT "non-melanoma"
    melanoma_index = (str_detect(rownames(dat_annrow), "melanoma") & !str_detect(rownames(dat_annrow), "non-melanoma"))
    
    dat_annrow[melanoma_index, "Cell Type"] = "Melanoma" # Changing melanoma cells to melanoma classification
  }
  
  
  
  # Creating heatmap
  
  
  dat_heat = ggheatmap(dat_mat,
                       scale = "row",
                       dist_method = "euclidean",
                       hclust_method = "complete",
                       dendrogram = "column",
                       main = paste0(str_to_title(treatment), "Cell Proportions"),
                       col_side_colors = dat_anncol,
                       row_side_colors = dat_annrow,
                       showticklabels = c(FALSE, TRUE))
  
  dat_heat_interactive = heatmaply(dat_mat,
                                   scale = "row",
                                   dist_method = "euclidean",
                                   hclust_method = "complete",
                                   dendrogram = "column",
                                   main = paste0(str_to_title(treatment), "Cell Proportions"),
                                   col_side_colors = dat_anncol,
                                   row_side_colors = dat_annrow,
                                   showticklabels = c(FALSE, TRUE))
  
  
  
  return(
    list(dat_heat = dat_heat, ## Note that ggheatmap gets returned anyway when we define it
         dat_heat_interactive = dat_heat_interactive)
  )
}