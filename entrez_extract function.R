require(rentrez)
require(tidyverse)

entrez_extract <- function(accessions){
  entrez_all <- entrez_summary(db="nucleotide", id=accessions)
  subnames <- extract_from_esummary(entrez_all, "subname") #extracting the subname - contains information about a particular strain
  subtypes <- extract_from_esummary(entrez_all, "subtype") #extracting the subtype - contains the categories of information
  subnames_split <- strsplit(subnames, split = "[|]")
  subtypes_split <- strsplit(subtypes, split = "[|]") #splitting the subnames and subtypes by |
  for(i in 1:length(subnames_split)) names(subnames_split[[i]]) <- subtypes_split[[i]] #connecting the subnames to their corresponding category
  variable_subset <- c("strain", "serotype", "host", "country", "segment", "collection_date") #telling the code which categories of information do we want to keep 
  subnames_split <- lapply(subnames_split, function(x) x[variable_subset]) #Extracting the desired categories 
  subnames_split <- do.call("rbind", subnames_split) 
  accessions <- extract_from_esummary(entrez_all, "caption") %>% 
    as.matrix() #extracting the accession numbers
  subnames_accessions <- cbind(accessions, subnames_split) #binding the accession numbers with the subnames information
  subnames_t <- as_tibble(subnames_accessions) %>% 
    rename(accesion_number = V1, location = country) #Making a tibble and renaming the columns 
  return(subnames_t)
}
