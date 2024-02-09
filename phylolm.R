require(tidyverse)
require(phylolm)
require(ape)

phylolm_analysis <- function(data, rates_column_base="rates"){
  trees <- list()
  phylolm_results <- list()
  phylolm_summaries <- list()
  phylolm_summaries_in <- list()
  df <- data[[1]] %>% as_tibble()
  
  rownames(df) <- df$label
  
  
  for (i in 1:length(data[[2]])) {
    tree <- data[[2]][[i]]
    
    rates_column <- paste0(rates_column_base, ".", i)
    
    formula1 <- as.formula(paste0(rates_column, " ~ mintemp + percipitation + mintemp:percipitation"))
    
    phylolm1 <- phylolm(formula1,
                           data = df, phy = tree)
   
     phylolm_summaries_in[[i]] <- summary(phylolm1)
    
    if(phylolm_summaries_in[[i]]$coef[4,1]<0) { #4,4 if the p-value is > 0.05 then run it without the interaction 
      formula2 <- as.formula(paste0(rates_column, " ~ mintemp + percipitation"))
      phylolm2 <- phylolm(formula2, 
                          data = df, phy = tree)
      print(paste0("Tree ", i, " had a negative association in the mintemp:percipitation interaction so the phylolm analysis was run again without the interaction"))
      phylolm_results[[i]] <- phylolm2
      phylolm_summaries[[i]] <- summary(phylolm2)
      } else {
        print(paste0("Tree ", i, " had a positive association with the in the mintemp:percipitation interaction, so the phylolm analysis results are saved with the interaction"))
      phylolm_results[[i]] <- phylolm1
      phylolm_summaries[[i]] <- summary(phylolm1)
    }
    
  }
  results_phylo <- list(
    phylolm_results = phylolm_results, 
    phylolm_summaries = phylolm_summaries
  )
  return(results_phylo)
}
