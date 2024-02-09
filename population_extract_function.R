require(tidyverse)
require(geodata)
require(raster)

population_extract <- function(data, path = "path_to_folder"){
  years <- unique(data$geodata_year)
  
  for(year in years) {
    population(year, res = "5", path = paste0(path))
    
  }
  
  for(i in 1:length(data$geodata_year)){
    pop_density <- raster(paste0(path, "/pop/gpw_v4_population_density_rev11_", data$geodata_year[i], "_5m.tif")) 
    data[data$geodata_year == data$geodata_year[i], "pop_density"] <- 
      raster::extract(pop_density, as.matrix(data[data$geodata_year == data$geodata_year[i], c("lon", "lat")]))
  }
  return(data)
}
