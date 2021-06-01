## Function to get barcode names/descriptions

get_barcodename = function(barcode){

  if(barcode == "00000000") {

    ## If showing none of the cellular phenotypes, name as "none"
    barcodename = "none"
  } else {

    barcode_index = barcode %>%
      str_split("") %>% # Splitting so that each 0 or 1 is its own element in vector
      .[[1]] %>% # Result of above is a list of length 1
      {. == "1"} # TRUE values for elements that are 1

    ## Assigning name based on phenotype expression separated by "/"
    barcodename = c("melanoma (sox10+)",
                    "cytotoxic t-cell (cd8+)",
                    "macrophage (cd68+)",
                    "cd16+ macrophages (cd68+)",
                    "cd16+ non-macrophages (cd68-)",
                    "all pd-l1",
                    "pd-l1 melanoma",
                    "pd-l1 non-melanoma")[barcode_index] %>%
      paste(collapse = "/")
  }

    return(barcodename)
}