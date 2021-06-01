
## Performing univariate t-tests and correcting p-value for gene expression

get_t.test_gene_expression = function(treatment, combdata, monodata, no_permutations){
  
  ## Defining data to use for the rest of the function
  if(treatment == "combination"){
    dat = combdata
  } else if(treatment == "monotherapy"){
    dat = monodata
  }
  
  
  
  # Performing permutation test
  
  
  ## Setting seed for reproducibility
  set.seed(2021)
  
  ## Defining number of permutations
  B = no_permutations
  
  ## Initialising list to store values
  perm_tstat = list()
  
  for(celltype in (dat %>% dplyr::select(-sample, -Treatment, -Response) %>% colnames())){
    
    permuted_dat = dat
    t_null = vector("numeric", B)
    t_formula = as.formula(paste0("`", celltype, "` ", "~ Response"))
    
    ## Obtaining true test statistic
    tt = t.test(t_formula, data = dat)$statistic
    
    ## Obtaining permuted test statistics
    for(i in 1:B){
      permuted_dat$response = sample(dat$response) #this is the permutation
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