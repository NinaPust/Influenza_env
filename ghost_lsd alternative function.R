require(tidyverse)
require(ape)

ghost_lsd_alt <- function(data, category, input_file, output_file_ghost, output_file_lsd){
  
  min_aic_value <- Inf
  best_category <- NULL
  
  aic_values<- list()
  site_probabilities <- list()
  substitution_process <- list()
  
  for (i in 1:length(category)){
    #print(category)
    ghost <- paste0("iqtree -s ", input_file, " -m GTR*H", category[i], " -bb 1000 -alrt 1000 -wspm -pre ", output_file_ghost, category[i])
    system(ghost)
    #print(ghost)
    
    site_probs <- read.table(paste0(output_file_ghost, category[i], ".siteprob"), header = TRUE) %>% as.matrix()
    site_probabilities[[i]] <- site_probs
    
    iqtree <- readLines(paste0(output_file_ghost, category[i], ".iqtree"))
    aic_line <- iqtree[grep("Akaike information criterion (AIC) score:", iqtree, fixed = TRUE)]
    aic_value <- gsub("[^0-9.-]", "", aic_line) %>% as.numeric()
    aic_values[i] <- aic_value
    #print(aic_value)
    if (aic_value < min_aic_value) {
      min_aic_value <- aic_value
      best_category <- category[i]
    }
    
    subs_file <- readLines(paste0(output_file_ghost, category[i], ".iqtree"))
    
    subs_text <- paste(subs_file, collapse = "\n")
    
    pattern <- "(?s)SUBSTITUTION PROCESS.*(?=MAXIMUM LIKELIHOOD TREE)"
    subs_ex_text <- regmatches(subs_text, regexpr(pattern, subs_text, perl = TRUE))
    
    # Print the extracted text
    subs_ex_text <- strsplit(subs_ex_text, "\n")[[1]]
    substitution_process[[i]] <- subs_ex_text
    
  }
  print(paste0("Category ", best_category, " has the lowest AIC value."))
  
  best_tree <- read.tree(paste0(output_file_ghost, best_category, ".treefile"))
  lsd_tree <- list()
  lsd_model_data <- list()
  edge_tibbles <- list() 
  
  for(i in 1:length(best_tree)) {
    #print(best_tree[[i]])
    ghost_edge <- edge_extract(best_tree[[i]]) 
    #print(edge_tibble)
    edge_tibble <- left_join(data, ghost_edge, by = "accession_number") 
    edge_tibble[[i]]
    #print(data_edge)
    tips_n_times <- edge_tibble[, c("label", "date")]
    write.table(tips_n_times[-1, ], file = (paste0(output_file_ghost, best_category, "_dates_tree", i, ".txt")), sep = "\t", col.names = FALSE, row.names = FALSE)
    
    
    write.tree(best_tree[[1]], file = paste0(output_file_ghost, best_category, "_tree", 1, ".treefile"))
    
    lsd <- paste0("iqtree2 -s ", input_file," --date ", output_file_ghost, best_category, "_dates_tree", i, ".txt", " -te ",output_file_ghost, best_category, "_tree", 1, ".treefile -pre ", output_file_lsd, "_tree_", i)
    #print(lsd)
    system(lsd) #This now runs LSD with edge lengths from every GHOST category but on the 1st tree of the best_tree list which is the summary tree
    
    
    lsd_tree[[i]] <- read.tree(paste0(output_file_lsd, "_tree_", i, ".treefile"))
    
    
    lsd_edge <- edge_extract(lsd_tree[[i]])
    #data_edge <- ghost_edge %>% left_join(lsd_edge [, c("Edge_length", "accession_number")], by="accession_number")
    data_edge <- ghost_edge %>% left_join(lsd_edge [, c("Edge_length", "accession_number")], by="accession_number")
    data_edge <- data_edge %>%
      rename_with(~paste0("edge.length.lsd"), matches("Edge_length.y")) %>%
      rename_with(~paste0("edge.length.ghost.", i), matches("Edge_length.x")) %>%
      mutate(!!paste0("rates.", i) := !!sym(paste0("edge.length.ghost.", i)) / !!sym(paste0("edge.length.lsd")))
    data_edge <- dplyr::select(data_edge, - Tip)
    
    #print(data_edge)
    edge_tibbles[[i]] <- data_edge
    
    lsd_report <- read_lines(paste0(output_file_lsd, "_tree_", i, ".iqtree")) 
    lsd_model_info <- lsd_report[grep( "Model                  LogL", lsd_report, fixed = TRUE)]
    lsd_model_data_line <- lsd_report[grep( "Model                  LogL", lsd_report, fixed = TRUE)+1]
    result <- c(lsd_model_info, lsd_model_data_line)
    
    lsd_model_data[[i]] <- result
    
  }
  
  join_tibbles <- function(x, y) {
    left_join(x, y, by = c("label", "accession_number"))
  }
  
  final_tibble <- Reduce(join_tibbles, c(list(edge_tibble), edge_tibbles)) %>% dplyr::select(- Tip, - Edge_length)
  
  
  results <- list(
    data_table = final_tibble,
    ghost_trees = best_tree,
    lsd_trees = lsd_tree,
    lsd_model_data = lsd_model_data, 
    site_probabilities = site_probabilities, 
    substitution_process = substitution_process #maybe save best AIC substitution only 
  )
  return(results)
}
