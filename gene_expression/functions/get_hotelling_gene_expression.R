
## Performing Hotelling's t-test for gene expression

get_hotelling_gene_expression = function(treatment, combdata, monodata, no_permutations){
  
  ## Defining data to use for the rest of the function
  if(treatment == "combination"){
    dat = combdata
  } else if(treatment == "monotherapy"){
    dat = monodata
  }
  
  
  
  # Obtaining data for analysis
  
  
  hotelling_dat = dat %>% 
    dplyr::select(-sample, -Treatment)
  
  
  
  # Performing Hotelling test
  
  
  ## Setting seed for reproducibility
  set.seed(2021)
  
  ## Performing analysis
  proptest = hotelling.test(
    .~Response,
    data = hotelling_dat,
    shrinkage = TRUE,
    perm = TRUE,
    B = no_permutations
  )
  
  
  
  # Plotting histogram of results
  
  
  hotelling_p = ggplot(data.frame(x = proptest$results)) +
    aes(x = x) +
    geom_histogram(colour = "black", fill = "grey") +
    geom_vline(xintercept = proptest$stats$statistic,
               colour = "red") +
    geom_text(mapping = aes(x = 40,
                            y = 100),
              label = paste("p =", proptest$pval),
              colour = "red") +
    labs(x = expression(T^2),
         y = "Count",
         title = paste0(str_to_title(treatment), ": Distribution of Permuted Test Statistics"),
         subtitle = "Covariance matrices estimated w/ James-Stein estimator\nn = 1000 permutations") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
  
  return(
    list(proptest = proptest, 
         hotelling_p = hotelling_p)
  )
}