require(tidyverse)

subnames_prepare_dates <- function(subnames_t){
  subnames_t <- subnames_t %>% separate(collection_date, 
                                        into = c("day", "month", "year"), 
                                        sep = "-", 
                                        remove = FALSE) #separating dates in the format 04-Dec-2000
  subnames_t <- subnames_t %>% separate(collection_date, 
                                        into = c("month2", "day2", "year2"), 
                                        sep = "/", 
                                        remove = FALSE) #separating dates in the format 12/04/2000
  month_mapping <- c(
    "Jan" = "01", "Feb" = "02", "Mar" = "03", "Apr" = "04",
    "May" = "05", "Jun" = "06", "Jul" = "07", "Aug" = "08",
    "Sep" = "09", "Oct" = "10", "Nov" = "11", "Dec" = "12") #Replacing word months with numbers
  subnames_t$month <- month_mapping[subnames_t$month]
  subnames_t <- subnames_t %>% 
    mutate(month = ifelse(is.na(month), month2, month
                          ), #if month is NA, replace it with month2 (for values that had the date in a different format)
           year = ifelse(is.na(year), year2, year)) %>%
    mutate(date = paste(year, month, sep = "-")
           ) %>%#Putting the date in the format of the .tif file we will need later
    mutate(month_decimal = as.numeric(month)/12) %>% 
    mutate(month_decimal = str_replace_all(as.character(month_decimal), "\\.", "")) %>%
    mutate(root_tip_date = paste(year, month_decimal, sep = ".")) %>% 
    dplyr::select(-day, -day2, -month, -month2, -year, -year2, -month_decimal) #removing unnecessary columns 
  print("Column root_tip_date contains a date where months are divided by 12 for easier root-to-tip lenght analysis later")
  return(subnames_t)
  
}
