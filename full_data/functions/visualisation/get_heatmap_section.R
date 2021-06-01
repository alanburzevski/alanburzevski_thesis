
## Obtaining heatmap for image sections

get_heatmap_section = function(section, treatment, combdata, monodata){
  
  ## Defining region-specific data to use for the rest of the function
  if(treatment == "combination"){
    region_dat = combdata %>% filter(region == section)
  } else if(treatment == "monotherapy"){
    region_dat = monodata %>% filter(region == section)
  }
  
  
  
  # Obtaining data in matrix form
  
  
  sectionprop_mat =
    region_dat %>% 
    dplyr::select(-imageID, -response, -batch, -region) %>% 
    as.matrix() %>% 
    t() %>% 
    
    # ## Sqrt transformation
    # apply(2, sqrt)
    
    ## Changing column names
    `colnames<-`(region_dat$imageID)
  
  
  
  # Obtaining annotations
  
  
  ## Column annotation: response and batch
  
  section_anncol = data.frame("Response" = region_dat$response,
                                   "Batch" = region_dat$batch) %>% 
    `rownames<-`(colnames(sectionprop_mat)) # Setting rownames to imageid (colnames of barcodeprop_mat)
  
  
  ## Row annotation: melanoma
  
  section_annrow = data.frame("Cell Type" = rep("Non-melanoma", nrow(sectionprop_mat))) %>%  # Populating with all Non-melanoma for now
    `colnames<-`("Cell Type") %>%  # For nicer plotting
    `rownames<-`(rownames(sectionprop_mat))  # Setting rownames to cell types (rownames of cellprop_mat)
  
  ## Obtaining row indexes which are "melanoma" AND NOT "non-melanoma"
  melanoma_index = (str_detect(rownames(section_annrow), "melanoma") & !str_detect(rownames(section_annrow), "non-melanoma"))
  
  section_annrow[melanoma_index, ] = "Melanoma" # Changing melanoma cells to melanoma classification
  
  
  
  # Creating heatmap
  
  
  section_heat = ggheatmap(sectionprop_mat,
                                scale = "row",
                                dist_method = "euclidean",
                                hclust_method = "complete",
                                dendrogram = "column",
                                main = paste(str_to_title(treatment), "Therapy Cell Proportions:", section),
                                col_side_colors = section_anncol,
                                row_side_colors = section_annrow,
                                showticklabels = c(FALSE, TRUE))
  
  section_heat_interactive = heatmaply(sectionprop_mat,
                                            scale = "row",
                                            dist_method = "euclidean",
                                            hclust_method = "complete",
                                            dendrogram = "column",
                                            main = paste(str_to_title(treatment), "Therapy Cell Proportions:", section),
                                            col_side_colors = section_anncol,
                                            row_side_colors = section_annrow,
                                            showticklabels = c(FALSE, TRUE))
  
  
  
  return(
    list(section_heat = section_heat, ## Note that ggheatmap gets returned anyway when we define it
         section_heat_interactive = section_heat_interactive)
  )
}