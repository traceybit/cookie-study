## tracey mangin
## april 12, 2023
## data exploration

library(tidyverse)
library(lubridate)
library(data.table)
library(here)

## paths
main_path <- "/Volumes/GoogleDrive/My Drive/cookie-research/"
# raw_d_path <- paste0(main_path, "data/raw/")
figs_path <- paste0(main_path, "figures/")

## files
survey_results <- "cookie-survey-responses-edit-id.csv"

## read in survey results
survey_df <- fread(here("data", "raw", survey_results))

## janitor to clean names
survey_df2 <- janitor::clean_names(survey_df) %>%
  select(-comments_questions)

## update column names
survey_cols <- c("timestamp", "fave_choc_chip", "fave_chipless", "fave_overall", "age", "emlab", "id")

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

## remove jen's entry
error_entry <- as.ITime("11:37:00")

survey_df3 <- survey_df3 %>%
  filter(time != error_entry)

## save
fwrite(survey_df3, paste(here("outputs", "processed", "clean_responses.csv")))


## fig 1 -- favorites, all
overall_df <- survey_df3 %>%
  select(emlab, age, fave_choc_chip, fave_chipless, fave_overall) %>%
  pivot_longer(fave_choc_chip:fave_overall, names_to = "category", values_to= "preference")

recipe_lut <- tibble(preference = c("Cookie A", "Cookie B", "Cookie C", "Cookie D", "No preference"),
                     recipe = c("LA Times: chips", "NYT: chips", "LA Times: chip-less", "NYT: chip-less", "No preference"),
                     recipe_general = c("LA Times", "NYT", "LA Times", "NYT", "No preference"))

overall_df2 <- overall_df %>%
  mutate(value = 1) %>%
  left_join(recipe_lut)

overall_df3 <- overall_df2 %>%
  group_by(category, recipe_general) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(category) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  mutate(perc = value / total * 100,
         perc_lab = paste0(round(perc), "%"),
         cat_label = ifelse(category == "fave_chipless", "Chip-less",
                            ifelse(category == "fave_choc_chip", "Chocolate chip", "Overall"))) 
  

overall_df3$cat_label <- factor(overall_df3$cat_label, levels = c("Chocolate chip", "Chip-less", "Overall"))
overall_df3$recipe_general <- factor(overall_df3$recipe_general, levels = c("No preference", "NYT", "LA Times"))


## figure
overall_fig <- ggplot(overall_df3, aes(fill = recipe_general, x = cat_label, y = value, label = perc_lab)) + 
  geom_bar(position = "fill", stat = "identity") +
  geom_text(position = position_fill(vjust=0.5), colour="white", size = 6) +
  labs(x = NULL,
       y = NULL) +
  theme_minimal() +
  scale_fill_manual(values= c("#f4a261", "#2a9d8f",  "#264653"),
                    guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16))

ggsave(overall_fig, 
       filename = paste0(figs_path, "overall_pref.png"),
       width = 7,
       height = 7,
       units = "in")
  
## chips values -- what % chipless pref for each?
chips_df <- overall_df2 %>%
  filter(category == "fave_overall") %>%
  mutate(chips_cat = ifelse(recipe %in% c("LA Times: chips", "NYT: chips"), "Chips",
                            ifelse(recipe == "No preference", "No preference", "Chip-less"))) %>%
  group_by(category, chips_cat) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(category) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  mutate(perc = value / total * 100,
         perc_lab = paste0(round(perc), "%"),
         cat_label = "Overall")

chips_df$chips_cat <- factor(chips_df$chips_cat, levels = c("No preference", "Chip-less", "Chips"))

## chips
chips_fig <- ggplot(chips_df, aes(fill = chips_cat, x = cat_label, y = value, label = perc_lab)) +
  geom_bar(position = "fill", stat = "identity") +
  geom_text(position = position_fill(vjust=0.5), colour="white", size = 6) +
  labs(x = NULL,
       y = NULL) +
  theme_minimal() +
  scale_fill_manual(values= c("#f4a261", "#C19A6B", "#3D2B1F"),
                    guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16))

ggsave(chips_fig, 
       filename = paste0(figs_path, "chips_pref.png"),
       width = 5,
       height = 7,
       units = "in")

## chip-less breakdown
chipless_df <- overall_df2 %>%
  filter(category == "fave_overall") %>%
  mutate(chips_cat = ifelse(recipe %in% c("LA Times: chips", "NYT: chips"), "Chips",
                            ifelse(recipe == "No preference", "No preference", "Chip-less"))) %>%
  filter(chips_cat == "Chip-less") %>%
  group_by(category, chips_cat, recipe_general) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(category) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  mutate(perc = value / total * 100,
         perc_lab = paste0(round(perc), "%"),
         cat_label = "Overall")

## break down overall, emlab/not emlab, kiddo/not kiddos
## --------------------------------------------------------------

overall_emlab <- overall_df2 %>%
  mutate(emlab = ifelse(emlab == "Yes", "emLab", "not emLab")) %>%
  group_by(category, emlab, recipe_general) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(category, emlab) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  mutate(perc = value / total * 100,
         perc_lab = paste0(round(perc), "%"),
         cat_label = ifelse(category == "fave_chipless", "Chip-less",
                            ifelse(category == "fave_choc_chip", "Chocolate chip", "Overall"))) 

overall_emlab$cat_label <- factor(overall_emlab$cat_label, levels = c("Chocolate chip", "Chip-less", "Overall"))
overall_emlab$recipe_general <- factor(overall_emlab$recipe_general, levels = c("No preference", "NYT", "LA Times"))

# n emlab, not emlab
n_emlab <- overall_df2 %>%
  mutate(emlab = ifelse(emlab == "Yes", "emLab", "not emLab")) %>%
  filter(category == "fave_overall") %>%
  group_by(emlab) %>%
  summarise(n = sum(value)) %>%
  ungroup() %>%
  mutate(total = sum(n),
         perc = n / total * 100) 
  
## emlab figure
emlab_fig <- ggplot(overall_emlab, aes(fill = recipe_general, x = emlab, y = value, label = perc_lab)) + 
  geom_bar(position = "fill", stat = "identity") +
  geom_text(position = position_fill(vjust=0.5), colour="white", size = 6) +
  facet_wrap(~cat_label) +
  labs(x = NULL,
       y = NULL) +
  theme_minimal() +
  scale_fill_manual(values= c("#f4a261", "#2a9d8f",  "#264653"),
                    guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 16))

ggsave(emlab_fig, 
       filename = paste0(figs_path, "emlab_pref.png"),
       width = 7,
       height = 7,
       units = "in")

## kiddos/ not kiddos
overall_kiddos <- overall_df2 %>%
  mutate(age = ifelse(age == "18 and up", ">=18", "<18")) %>%
  group_by(category, age, recipe_general) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(category, age) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  mutate(perc = value / total * 100,
         perc_lab = paste0(round(perc), "%"),
         cat_label = ifelse(category == "fave_chipless", "Chip-less",
                            ifelse(category == "fave_choc_chip", "Chocolate chip", "Overall"))) 

overall_kiddos$age <- factor(overall_kiddos$age, levels = c("<18", ">=18"))
overall_kiddos$cat_label <- factor(overall_kiddos$cat_label, levels = c("Chocolate chip", "Chip-less", "Overall"))
overall_kiddos$recipe_general <- factor(overall_kiddos$recipe_general, levels = c("No preference", "NYT", "LA Times"))

# n age
n_age <- overall_df2 %>%
  mutate(age = ifelse(age == "18 and up", ">=18", "<18")) %>%
  filter(category == "fave_overall") %>%
  group_by(age) %>%
  summarise(n = sum(value)) %>%
  ungroup() %>%
  mutate(total = sum(n),
         perc = n / total * 100) 


## age figure
age_fig <- ggplot(overall_kiddos, aes(fill = recipe_general, x = age, y = value, label = perc_lab)) + 
  geom_bar(position = "fill", stat = "identity") +
  geom_text(position = position_fill(vjust=0.5), colour="white", size = 6) +
  facet_wrap(~cat_label) +
  labs(x = NULL,
       y = NULL) +
  theme_minimal() +
  scale_fill_manual(values= c("#f4a261", "#2a9d8f",  "#264653"),
                    guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 16))

ggsave(age_fig, 
       filename = paste0(figs_path, "age_pref.png"),
       width = 7,
       height = 7,
       units = "in")

## cookie fidelity
## -----------------------------------------

fidelity_df <- survey_df3 %>%
  select(emlab, age, fave_choc_chip, fave_chipless) %>%
  mutate(fave_choc_chip = ifelse(fave_choc_chip == "Cookie A",  "LA Times", "NYT"),
         fave_chipless = ifelse(fave_chipless == "Cookie C", "LA Times", "NYT"),
         fidelity = ifelse(fave_choc_chip == fave_chipless, 1, 0),
         switch = ifelse(fidelity == 1, "fidelity",
                         ifelse(fidelity == 0 & fave_chipless == "NYT", "LA Times chips --> NYT chip-less",
                                "NYT chips --> LA Times chips")))

## overall
overall_fid <- fidelity_df %>%
  mutate(value = 1) %>%
  group_by(fidelity) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(total = sum(value)) %>%
  mutate(perc = value / total * 100,
         perc_lab = paste0(round(perc), "%"))

emlab_fid <- fidelity_df %>%
  mutate(value = 1) %>%
  group_by(emlab, fidelity) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(emlab) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  mutate(perc = value / total * 100,
         perc_lab = paste0(round(perc), "%")) %>%
  mutate(emlab = ifelse(emlab == "Yes", "emLab", "not emLab"),
         fidelity = ifelse(fidelity == 1, "loyal", "switch")) 

emlab_fid$fidelity <- factor(emlab_fid$fidelity , levels = c("switch", "loyal"))

## emlab figure
emlab_fidel_fig <- ggplot(emlab_fid, aes(fill = fidelity, x = emlab, y = value, label = perc_lab)) + 
  geom_bar(position = "fill", stat = "identity") +
  geom_text(position = position_fill(vjust=0.5), colour="white", size = 6) +
  labs(x = NULL,
       y = NULL) +
  theme_minimal() +
  scale_fill_manual(values= c("#ffc300", "#0081AF"),
                    guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 16))

ggsave(emlab_fidel_fig, 
       filename = paste0(figs_path, "emlab_loyalty.png"),
       width = 5,
       height = 7,
       units = "in")

## kiddo fidel
kiddo_fid <- fidelity_df %>%
  mutate(value = 1) %>%
  group_by(age, fidelity) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(age) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  mutate(perc = value / total * 100,
         perc_lab = paste0(round(perc), "%")) %>%
  mutate(age = ifelse(age == "18 and up", ">=18", "<18"),
         fidelity = ifelse(fidelity == 1, "loyal", "switch")) 

kiddo_fid$fidelity <- factor(kiddo_fid$fidelity , levels = c("switch", "loyal"))
kiddo_fid$age <- factor(kiddo_fid$age, levels = c("<18", ">=18"))

## emlab figure
kiddo_fidel_fig <- ggplot(kiddo_fid, aes(fill = fidelity, x = age, y = value, label = perc_lab)) + 
  geom_bar(position = "fill", stat = "identity") +
  geom_text(position = position_fill(vjust=0.5), colour="white") +
  labs(x = NULL,
       y = NULL) +
  theme_minimal() +
  # scale_fill_manual(values= c("#f4a261", "#2a9d8f",  "#264653"),
  #                   guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(size = 10))

## classify the type of switch
switch_type <- fidelity_df %>%
  filter(fidelity == 0) %>%
  group_by(switch) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(total = sum(n)) %>%
  mutate(perc = n / total * 100,
         perc_lab = paste0(round(perc), "%"))

## time --------------------------------
## -------------------------------------

time_df <- survey_df3 %>%
  select(emlab, age, fave_choc_chip, fave_chipless, fave_overall, hours) %>%
  pivot_longer(fave_choc_chip:fave_overall, names_to = "category", values_to= "preference") %>%
  left_join(recipe_lut) %>%
  mutate(cat_label = ifelse(category == "fave_chipless", "Chip-less",
                            ifelse(category == "fave_choc_chip", "Chocolate chip", "Overall"))) %>%
  select(cat_label, recipe_general, hours) %>%
  mutate(value = 1) %>%
  arrange(cat_label, recipe_general, hours) %>%
  group_by(cat_label, recipe_general, hours) %>%
  summarise(value = sum(value)) %>%
  ungroup() 

## expand grid
time_expand <- expand.grid(cat_label = unique(time_df$cat_label),
                           recipe_general = unique(time_df$recipe_general),
                           hours = unique(time_df$hours))

## join
time_df2 <- time_expand %>%
  left_join(time_df) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  arrange(cat_label, recipe_general, hours) %>%
  group_by(cat_label, recipe_general) %>%
  mutate(cumul_like = cumsum(value)) %>%
  select(cat_label, recipe_general, hours, cumul_like)

## 
time_df2$cat_label <- factor(time_df2$cat_label, levels = c("Chocolate chip", "Chip-less", "Overall"))
time_df2$recipe_general <- factor(time_df2$recipe_general, levels = c("No preference", "NYT", "LA Times"))

time_fig <- ggplot(time_df2 %>% filter(cat_label == "Chocolate chip"), aes(x = hours, y = cumul_like, color = recipe_general)) +
  geom_line(size = 3, alpha = 0.9) +
  facet_wrap(~cat_label, ncol = 1) +
  geom_vline(xintercept = 16, size = 0.5, lty = "dashed", color = "black") +
  annotate("text", x = 15.5, y = 15, label = "4pm first\nnon-emLab\nresponses", size = 5, hjust = 1) +
  labs(x = "Approximate hours from bake",
       y = "Cumulative likes") +
  theme_minimal() +
  xlim(10, 45) +
  scale_color_manual(values= c("#f4a261", "#2a9d8f",  "#264653"),
                     guide = guide_legend(reverse = TRUE)) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 16))

ggsave(time_fig, 
       filename = paste0(figs_path, "pref_time.png"),
       width = 9,
       height = 7,
       units = "in")













