
## Performing univariate t-tests and correcting p-value for image sections

get_t.test_section = function(section, treatment, combdata, monodata, no_permutations){
  
  ## Defining region-specific data to use for the rest of the function
  if(treatment == "combination"){
    region_dat = combdata %>% filter(region == section)
  } else if(treatment == "monotherapy"){
    region_dat = monodata %>% filter(region == section)
  }
  
  
  
  # Performing permutation test
  
  
  ## Setting seed for reproducibility
  set.seed(2021)
  
  ## Defining number of permutations
  B = no_permutations
  
  ## Initialising list to store values
  perm_tstat = list()
  
  for(celltype in (region_dat %>% dplyr::select(-batch, -response, -region, -imageID) %>% colnames())){
    
    permuted_dat = region_dat
    t_null = vector("numeric", B)
    t_formula = as.formula(paste0("`", celltype, "` ", "~ response"))
    
    ## Obtaining true test statistic
    tt = t.test(t_formula, data = region_dat)$statistic
    
    ## Obtaining permuted test statistics
    for(i in 1:B){
      permuted_dat$response = sample(region_dat$response) #this is the permutation
      t_null[i] = t.test(t_formula, data = permuted_dat)$statistic
    }
    
    
    
    # Obtaining p-values
    
    
    pval = mean(abs(t_null) >= abs(tt))
    
    perm_tstat[[celltype]] = list(tt = tt, 
                                  t_null = t_null, 
                                  pval = pval)
    
  }
  
  return(perm_tstat)
  
}