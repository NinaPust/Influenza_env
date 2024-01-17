require(tidyverse)
require(phangorn)

edge_extract <- function(tree) {
  ntips <- Ntip(tree)
  tips <- tree$edge[,2]
  edge_lenght <- tree$edge.length
  tip_tibble <- tibble(Tip = tips, Edge_length = edge_lenght)
  tip_tibble <- tip_tibble %>% filter(Tip >= 1, Tip <= ntips) #Filters out all the nodes, only leaving the tips 
  tip_label_tibble <- tibble(label=tree$tip.label)
  tip_label_tibble <- tip_label_tibble %>% mutate(accession_number=word(label, 1, sep="_"), Tip = row_number()) #The accession number can be used later to join the tibble with the main tibble
  tip_tibble_final <- left_join(tip_tibble, tip_label_tibble, by="Tip")
  return(tip_tibble_final)
}
