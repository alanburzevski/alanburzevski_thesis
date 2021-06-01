
## Data cleaning function


data_clean = function(batch_data) {
  
  
  
  # Removing proximity analysis rows
  
  
  ## Detecting for "Cl" in the analysis input
  ## Cl because rows of interest contain analysis input = "Class list ..."
  rows_idx = batch_data$`Analysis Inputs` %>% str_detect(fixed("Cl"))
  
  ## Verification that detecting Cl isolates only the necessary rows
  # batch_data$`Analysis Inputs` %>% unique() %>% str_detect(fixed("Cl"))
  
  ## Obtaining only those rows informative rows
  batch_data_cells = batch_data[rows_idx, ] %>% 
    select(-`Analysis Inputs`)
  
  
  
  # Replacing image location with image ID
  
  
  ## Function to remove certain characters
  loc_to_id = function(x){
    
    ## If the location contains batch 3&4 stiched images (like in batch3_4)
    if(str_detect(x[1], "Batch 3&4 stiched images")){
      
      x %>% 
        str_replace("E:\\\\Personalised ICB\\\\Batch 3&4 stiched images\\\\", "") %>% # Removing the parent directories "E:\\Personalised ICB\\"
        str_replace(".tif", "") # Removing the .tif
      
      ## If the location contains batch 5 stitched images (like in batch5)
    } else if(str_detect(x[1], "Batch 5 Stitched Images")){
      
      x %>% 
        str_replace("D:\\\\Tuba\\\\Batch 5 Stitched Images\\\\", "") %>% # Removing the parent directories "E:\\Personalised ICB\\"
        str_replace(".tif", "") # Removing the .tif
      
      ## If the location contains batch 6 stitched images (like in batch6)
    } else if(str_detect(x[1], "Batch 6 Stitched Images")){
    
    x %>% 
      str_replace("D:\\\\Tuba\\\\Batch 6 Stitched Images\\\\", "") %>% # Removing the parent directories "E:\\Personalised ICB\\"
      str_replace(".tif", "") # Removing the .tif
      
      ## If the location contains batch 7 stitched images (like in batch7)
    } else if(str_detect(x[1], "Batch 7 Stitched Images")){
      
      x %>% 
        str_replace("D:\\\\Tuba\\\\Batch 7 Stitched Images\\\\", "") %>% # Removing the parent directories "E:\\Personalised ICB\\"
        str_replace(".tif", "") # Removing the .tif
      
      ## If the location contains batch 8&9 stitched images (like in batch6)
    } else if(str_detect(x[1], "Batch 8&9_Stitched Images")){
      
      x %>% 
        str_replace("D:\\\\Batch 8&9_Stitched Images\\\\", "") %>% # Removing the parent directories "E:\\Personalised ICB\\"
        str_replace(".tif", "") # Removing the .tif
      
      ## Else if it doesn't
    } else {
      
      x %>% 
        str_replace("E:\\\\Personalised ICB\\\\", "") %>% # Removing the parent directories "E:\\Personalised ICB\\"
        str_replace(".tif", "") # Removing the .tif
      
    }
  }
  
  ## Adding imageID and removing image location
  batch_data_cells = batch_data_cells %>% 
    mutate(imageID = loc_to_id(batch_data_cells$`Image Location`)) %>% 
    select(-`Image Location`)
  
  
  
  # Replacing X and Y columns with mean(Xmin, Xmax)
  
  
  ## Function to take mean of X and Y
  xy_fun = function(dat){
    dat %>% 
      mutate(x = dat %>% 
               select(XMin, XMax) %>% 
               rowMeans() %>% 
               round(),
             y = dat %>% 
               select(YMin, YMax) %>% 
               rowMeans() %>% 
               round())
  }
  
  ## Adding x and y and removing XMin, XMax, YMin, YMax
  batch_data_cells = batch_data_cells %>% 
    xy_fun() %>% 
    select(-`XMin`, -`XMax`, -`YMin`, -`YMax`)
  
  
  
  # Removing unnecessary columns
  
  
  ## Removing analysis region column
  if("Analysis Region" %in% colnames(batch_data_cells)){
    batch_data_cells = batch_data_cells %>% 
      select(-`Analysis Region`) # All values are "Layer 1" so no need for this column
  }
  
  ## Removing distance column (batch 3_4)
  if(sum(str_detect(colnames(batch_data_cells), "Distance")) != 0){
    batch_data_cells = batch_data_cells %>% 
      select_if(!str_detect(colnames(batch_data_cells), "Distance"))
  }
  
  ## Removing region area column (batch 5)
  if(sum(str_detect(colnames(batch_data_cells), "Region Area")) != 0){
    batch_data_cells = batch_data_cells %>% 
      select_if(!str_detect(colnames(batch_data_cells), "Region Area"))
  }
  
  ## Removing region perimeter column (batch 5)
  if(sum(str_detect(colnames(batch_data_cells), "Region Perimeter")) != 0){
    batch_data_cells = batch_data_cells %>% 
      select_if(!str_detect(colnames(batch_data_cells), "Region Perimeter"))
  }
  
  ## Removing proximity analysis columns
  batch_data_cells = batch_data_cells %>% 
    select_if(!str_detect(colnames(batch_data_cells), "= '1'")) # Proximity columns are those with "= '1'" in colname
  
  
  ## Changing all columns to lowercase to remove naming discrepancies
  colnames(batch_data_cells) = tolower(colnames(batch_data_cells))
  
  
  ## Changing batch 8&9 column name to match the other batches --> confirmed with Tuba to do this
  if("cd16+ macrophages (cd68-)" %in% colnames(batch_data_cells)) {
    colnames(batch_data_cells)[which(colnames(batch_data_cells) == "cd16+ macrophages (cd68-)")] = "cd16+ non-macrophages (cd68-)"
  }
  
  return(batch_data_cells)
}
