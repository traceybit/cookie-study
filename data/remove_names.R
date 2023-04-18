## tracey mangin
## april 17, 2023
## create data without names for public repo

library(data.table)
library(tidyverse)

## paths
main_path <- "/Volumes/GoogleDrive/My Drive/cookie-research/"
raw_d_path <- paste0(main_path, "data/raw/")

## file name
survey_results <- "cookie-survey-responses-edit.csv"

## read in from google drive
results_df <- fread(paste0(raw_d_path, survey_results))

## add in id
results_df$id <- paste0("R", 1:nrow(results_df))

## save in gdrive
fwrite(results_df, paste0(raw_d_path, "cookie-survey-responses-edit-name-id.csv"))


## remove names
results_df <- results_df %>%
  select(-'Name (optional)')

## save in github
fwrite(results_df, paste(here("data", "raw", "cookie-survey-responses-edit-id.csv")))

