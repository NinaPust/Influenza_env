require(tidyverse)
require(ggmap)

subnames_prepare_locations <- function(subnames_t){
  bad_samples <- subnames_t %>% 
    filter(is.na(strain))
  subnames_t <- subnames_t %>% 
    filter(!is.na(strain)) %>% #filters out any rows that have strain name NA 
    separate(location, 
             into = c("Country", "Town"), 
             sep = ":", 
             extra = "merge") %>% #Splits the location into two new columns
    mutate(strain_location = word(strain, 2, sep = "/")) %>% #Extracts the location from the strain name 
    mutate(Town = ifelse(is.na(Town) & strain_location != Country, 
                         strain_location, Town), 
           Town = ifelse(strain_location == Country, 
                         NA, Town)) %>% ##If town is NA and strain_location isn't the same as the country, then copy the contents of strain_location into town
    mutate(Country = ifelse(is.na(Country), 
                            strain_location, Country)) %>% #if Country is NA, strain_location will be copied to country
    mutate(geocode_location = ifelse(is.na(Town),Country,
                                     paste(Town, Country, 
                                           sep = ", "))) #a new column that puts the location in the format readable by geocode
  
  if(nrow(bad_samples) >0) {
  print(paste0("Samples with accession numbers ", paste(bad_samples$accession_number, collapse = ", "), " were removed due to missing a strain name."))
  }
  
  geocode_info <- subnames_t %>% dplyr::select(accession_number, geocode_location)
  geocode_info <- geocode_info %>%
    rowwise() %>%
    mutate(geo_info = list(geocode(geocode_location))) %>%
    unnest(geo_info) %>% 
    dplyr::select(!geocode_location)
  subnames_final <- left_join(subnames_t, geocode_info, by = "accession_number")
  return(subnames_final)
  
}
