require(raster)
require(tidyverse)

temperature_extract <- function(data, min_temp_path = "path_to_folder", max_temp_path = "path_to_folder", percipitation_path="path_to_folder") {
  dates <- unique(data$date) #extracts unique dates
  data$mintemp <- NA #Creates 3 empty columns for minimum temperature, maximum temperature and percipitation
  data$maxtemp <- NA
  data$percipitation <- NA
  for(i in 1:length(dates)) {
    mintemp <- raster(paste0(min_temp_path, "/wc2.1_5m_tmin_", dates[i], ".tif")) #Extracts only the .tif files that contain dates found in the date column
    maxtemp <- raster(paste0(max_temp_path, "/wc2.1_2.5m_tmax_", dates[i], ".tif"))
    percipitation <- raster(paste0(percipitation_path, "/wc2.1_2.5m_prec_", dates[i], ".tif"))
      
    data[data$date==dates[i],"mintemp"] <- raster::extract(mintemp, as.matrix(data[data$date==dates[i],c("lon", "lat")])) #Extracts the environmental information from .tif files based on the coordinates
    data[data$date==dates[i], "maxtemp"] <- raster::extract(maxtemp, as.matrix(data[data$date==dates[i],c("lon", "lat")]))
    data[data$date==dates[i], "percipitation"] <- raster::extract(percipitation, as.matrix(data[data$date==dates[i],c("lon", "lat")]))
   
  }
  bad_samples <- data %>% filter(is.na(mintemp))
  data <- data %>% filter(!is.na(mintemp))
  
  if(nrow(bad_samples) >0) {
    print(paste0("Samples with accession numbers ", paste(bad_samples$accession_number, collapse = ","), " were removed due to no environmental data being avaliable at their location."))
  }
  return(data)
}
  
  
