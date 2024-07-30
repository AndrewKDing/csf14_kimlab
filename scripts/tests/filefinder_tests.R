source("./filefinder.R")
library(tidyverse)
library(testthat)

# check filefinding function, should give 46 obs
dir_test <- function() {
  cohort_list <- dir_finder("data/raw", "CFR02")
  test_that(
    "Checking correct file finding behavior",
    expect_equal(nrow(cohort_list), 46)
  )
}

# There should only be one date and one animal id for each file
id_test <- function(filenames) {
  count_dates <- length(str_extract_all(filenames, "\\d{8}"))
  count_names <- length(str_extract_all(filenames, "\\d{5}"))

  test_that(
    "Checking that only one date and only one animal name is pulled per file",
    expect_equal(count_dates, count_names)
  )
}
