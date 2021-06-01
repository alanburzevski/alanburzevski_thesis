
## Performing Hotelling's t-test for image sections

get_hotelling_section = function(section, treatment, combdata, monodata, no_permutations){
  
  ## Defining region-specific data to use for the rest of the function
  if(treatment == "combination"){
    region_dat = combdata %>% filter(region == section)
  } else if(treatment == "monotherapy"){
    region_dat = monodata %>% filter(region == section)
  }
  
  
  
  # Obtaining data for analysis
  
  
  hotelling_dat = region_dat %>% 
    dplyr::select(-imageID, -batch, -region)
  
  
  
  # Performing Hotelling test
  
  
  ## Setting seed for reproducibility
  set.seed(2021)

  ## Performing analysis
  proptest = hotelling.test(
    .~response,
    data = hotelling_dat,
    shrinkage = TRUE,
    perm = TRUE,
    B = no_permutations
  )
  
  
  
  # Plotting histogram of results
  
  
  hotelling_p = ggplot(data.frame(x = proptest$results)) +
    aes(x = x, y = ..density..) +
    geom_histogram(colour = "black", fill = "grey") +
    geom_vline(xintercept = proptest$stats$statistic,
               colour = "red") +
    geom_text(mapping = aes(x = 40,
                            y = 100),
              label = paste("p =", proptest$pval),
              colour = "red") +
    labs(x = expression(paste("Distribution of permutation Hotelling's ", T^2, " statistics")),
         y = "Density",
         title = paste0(str_to_title(treatment), " Therapy (", section, ")"),
         subtitle = paste("n =", no_permutations, "permutations")) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
  
  return(
    list(proptest = proptest, 
         hotelling_p = hotelling_p)
  )
}