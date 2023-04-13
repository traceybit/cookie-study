## tracey mangin
## april 12, 2023
## data exploration

library(tidyverse)
library(lubridate)
library(data.table)

## paths
main_path <- "/Volumes/GoogleDrive/My Drive/cookie-research/"
raw_d_path <- paste0(main_path, "data/raw/")

## files
survey_results <- "cookie-survey-responses-edit.csv"

## read in survey results
survey_df <- fread(paste0(raw_d_path, survey_results))

## janitor to clean names
survey_df2 <- janitor::clean_names(survey_df) %>%
  select(-comments_questions)

## update column names
survey_cols <- c("timestamp", "fave_choc_chip", "fave_chipless", "fave_overall", "age", "emlab", "name")

colnames(survey_df2) <- survey_cols

## make cleaner names for cookies
survey_df2 <- survey_df2 %>%
  mutate(fave_choc_chip = str_remove(fave_choc_chip, "(if cookies came stacked, top cookie)"),
         fave_choc_chip = str_remove(fave_choc_chip, "(if cookies came stacked, second from top cookie)"),
         fave_choc_chip = gsub("[()]", "", fave_choc_chip),
         fave_choc_chip = str_trim(fave_choc_chip, side = "both"),
         fave_chipless = str_remove(fave_chipless, "(if cookies came stacked, bottom cookie)"),
         fave_chipless = str_remove(fave_chipless, "(if cookies came stacked, third from top cookie)"),
         fave_chipless = gsub("[()]", "", fave_chipless),
         fave_chipless = str_trim(fave_chipless, side = "both"))

# specifying date format format
format <- "%Y-%m-%d %H:%M:%S"

## add columns for date and time
start_date <- as.Date("2023-04-04")
start_time <- as.ITime("00:00")
start_date_time <- as.POSIXct(paste(start_date, start_time), format = format)

survey_df3 <- survey_df2 %>%
  mutate(date = substr(timestamp, 1, 6),
         date = as.Date(date, "%m/%d/%y"),
         time = substr(timestamp, 8, 12),
         time = as.ITime(time),
         date_time = as.POSIXct(paste(date, time), format = format),
         time_diff = difftime(date_time, start_date_time, units = "hours"),
         hours = round(as.numeric(time_diff)))

## fig 1 -- favorites, all
overall_df <- survey_df3 %>%
  select(emlab, age, fave_choc_chip, fave_chipless, fave_overall) %>%
  pivot_longer(fave_choc_chip:fave_overall, names_to = "category", values_to= "preference")

recipe_lut <- tibble(preference = c("Cookie A", "Cookie B", "Cookie C", "Cookie D", "No preference"),
                     recipe = c("LA Times: chips", "NYT: chips", "LA Times: chip-less", "NYT: chip-less", "No preference"),
                     recipe_general = c("LA Times", "NYT", "LA Times", "NYT", "No preference"))

overall_df2 <- overall_df %>%
  mutate(value = 1) %>%
  left_join(recipe_lut) %>%
  group_by(category, recipe, recipe_general) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(category) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  mutate(perc = value / total * 100,
         perc_lab = paste0(round(perc), "%"),
         cat_label = ifelse(category == "fave_chipless", "Chip-less",
                            ifelse(category == "fave_choc_chip", "Chocolate chip", "Overall"))) 
  

overall_df2$cat_label <- factor(overall_df2$cat_label, levels = c("Chocolate chip", "Chip-less", "Overall"))
overall_df2$recipe <- factor(overall_df2$recipe, levels = c("No preference", "NYT: chip-less", "LA Times: chip-less",
                                                            "NYT: chips", "LA Times: chips"))

## figure
overall_fig <- ggplot(overall_df2, aes(fill = recipe, x = cat_label, y = value, label = perc_lab)) + 
  geom_bar(position = "fill", stat = "identity") +
  geom_text(position = position_fill(vjust=0.5), colour="white") +
  labs(x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank())
  
  
  
  