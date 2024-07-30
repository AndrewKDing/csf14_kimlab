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

#dir_finder returns directories with desired cohort
dir_finder <- function(pathname, cohort) {
  file_list <- list.dirs(path = pathname)
  cohort_list <- grep(paste0(cohort), file_list, value=TRUE)
  
  #Not sure what the autogating folders are doing here
  cohort_list <- as_tibble(cohort_list)
  cohort_list <- cohort_list %>%
    filter(!str_detect(value, ".autoGate"))
  
  return(cohort_list)
}

#fthis should be used in the main script
lapply(cohort_list, list.files)

# filename_extract pulls data from filename. It looks for date, an 8 digit string,
# then looks for animal id, a 5 digit string.
filename_extract <- 






