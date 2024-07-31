# I have a million files. The purpose of this script is to make a list of the ones I need to analyze

# Script arguments: directory to look in, cohort
# Logic
# 1. Make a list of all the directories
# 2. Find directories that correspond to the appropriate cohort
# 3. Get a list of all the files in these directories, turn it into a tibble
# 4. Extract animal ID and date from these files. Add these as columns to the tibble
#     Date is always the first 8 numbers in the filename
#     Animal ID is six numbers long, and surrounded by underscores
# 5. For each entry, find the infection date of the animal, add it to the tibble
# 6. Calculate the DPI of each timepoint
# 7. Create another tibble that is filename, animal ID, and DPI

library(tidyverse)
library(readxl)
library(stringr)

# dir_finder returns directories with desired cohort
dir_finder <- function(pathname, cohort) {
  file_list <- list.dirs(path = pathname)
  cohort_list <- grep(paste0(cohort), file_list, value = TRUE)

  # Not sure what the autogating folders are doing here
  cohort_list <- as_tibble(cohort_list)
  cohort_list <- cohort_list %>%
    filter(!str_detect(value, ".autoGate"))

  return(cohort_list)
}

# filename_extract pulls data from filename. It looks for date, an 8 digit string,
# then looks for animal id, a 5 digit string.
# returns a tibble with [filename, animal id, date]
# assumes the MMDDYYY format used by Andradi
filename_extract <- function(filenames, format = NULL) {
  if (is.null(format)){
    format <-  "%m%d%Y"
  } else {
    format <-  format
  }
  
  date <- str_extract(filenames[[1]], "\\d{8}")
  animal_id <- as.factor(str_extract(filenames[[1]], "\\d{5}"))

  df <- tibble(filenames[[1]], animal_id, date)
  
  #convert the date into an actual date
  df <- df %>%
    mutate(date = as.Date(date, "%m%d%Y"))
}

#calculates dpi, takes the cohort table and a tibble that has animal ids
#datevar and idvar are the names of the variables in the id and lookup dataframes,
# respectively. 

#Specifies names of variables for idvar and datevar, expects variable names
# to be the same
calculate_dpi <- function(ids_df, 
                          lookup_df,
                          idvar,
                          datevar
                          ){
  
  #Only infection date is required from the lookup
  lookup_df <- lookup_df %>%
    select(idvar, datevar)
  
  #Joining the dataframes
  dpi_df <- left_join(ids_df, lookup_df, by=c(idvar, datevar))
}
