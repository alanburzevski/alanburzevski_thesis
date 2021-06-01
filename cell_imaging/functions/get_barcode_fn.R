## Function to get barcodes

get_barcode = function(batchdata){

  barcode = batchdata %>%
    select_if(colnames(batchdata) %in% celltype_cols) %>%
    apply(1, function(x) paste(x, collapse = "")) # Pasting all values across columns for each row
}