## Cellprop function
## Obtains the cell proportions

## Function to get cell proportions
get_cellprop = function(batchdata) {

  ## Obtaining raw counts

  ## Initialising list
  typecount = list()

  ## Loop to obtain counts per ID
  for(id in batchdata %>% select(imageid) %>% distinct() %>% pull()){

    typecount[[id]] = batchdata %>%

      ## Filtering by patient
      filter(imageid == id) %>%

      ## Selecting appropriate columns
      select_if(colnames(batchdata) %in% celltype_cols) %>%

      ## Obtaining counts
      colSums()
  }


  ## Obtaining proportions
  proplist = lapply(typecount, function(x) x %>% `/`(sum(x)))

  ## Obtaining the batch this is from
  batchname = deparse(substitute(batchdata))

  typeprop = proplist %>%
    bind_rows() %>%
    mutate(imageid = names(proplist),
           batch = batchname) # deparse(substitute()) will return batchdata as a string

  return(typeprop)
}