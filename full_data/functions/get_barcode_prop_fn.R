## Obtaining proportions for barcoded datasets

get_barcode_prop = function(batchdata){
  
  ## Obtaining the batch this is from
  batchname = deparse(substitute(batchdata)) %>%
    str_replace(pattern = "_barcode",
                replace = "")
  
  batchdata %>%
    modify_if(is.character, as.factor) %>%
    group_by(imageid, celltype, .drop = FALSE) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    group_by(imageid, .drop = FALSE) %>%
    mutate(prop = count/sum(count)) %>%
    mutate(batch = batchname)
}