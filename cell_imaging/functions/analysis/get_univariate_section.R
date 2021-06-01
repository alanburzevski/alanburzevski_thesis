
# Get univariate significant cellproportions

get_univariate_section = function(no_permutations = 10000, regionname, data){
  
  ## Section data
  sectionprop_univariate = data %>% filter(region == regionname)
  
  ## Setting seed for reproducibility
  set.seed(2021)
  
  ## Defining number of permutations
  B = no_permutations
  
  ## Initialising list to store values
  perm_tstat = list()
  
  for(celltype in (sectionprop_univariate %>% select(-batch, -response, -imageID, -region) %>% colnames())){
    
    permuted_dat = sectionprop_univariate
    t_null = vector("numeric", B)
    t_formula = as.formula(paste0("`", celltype, "` ", "~ response"))
    
    ## Obtaining true test statistic
    tt = t.test(t_formula, data = sectionprop_univariate)$statistic
    
    ## Obtaining permuted test statistics
    for(i in 1:B){
      permuted_dat$response = sample(sectionprop_univariate$response) # Permuting
      t_null[i] = t.test(t_formula, data = permuted_dat)$statistic # Obtaining test statistic
    }
    
    ## Obtaining p-value (two-sided test)
    pval = mean(abs(t_null) >= abs(tt))
    
    perm_tstat[[celltype]] = list(tt = tt, t_null = t_null, pval = pval)
    
  }
  
  ## Obtaining significant cell types
  ## Significance level of 0.05 using bonferroni correction
  sectionprop_sig = perm_tstat %>% sapply(function(x) x$pval) %>% sort() %>%
    `*`(sectionprop_univariate %>% select(-batch, -response, -imageID, -region) %>% colnames() %>% length()) %>%
    .[{which(. < 0.05)}]
  
  return(list(
    values = perm_tstat,
    significant_celltypes = sectionprop_sig
  ))
  
}
