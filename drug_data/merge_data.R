#test on the DiD
rm(list=ls())
library(dplyr)
library(readxl)
library(lubridate)
library(did)
library(fixest)
################################################
panel_dataset <- read.csv("~/Desktop/Data/drug_data/panel_data.csv")
panel_both <- read.csv("~/Desktop/Data/drug_data/panel_both.csv")
#extract the drug id and firm id from panel_dataset, distinct
# drug_id <- panel_both %>% select(Drug_ID, Name) %>% distinct()
# firm_id <- panel_both %>% select(Firm_ID, Firm) %>% distinct()

# generate a drug_id in panel_dataset
panel_dataset <- panel_dataset %>% select(-X)
#generate the drug and Firm ID
panel_dataset <- panel_dataset %>%
  mutate(Drug_ID = as.numeric(factor(Name)))
panel_dataset <- panel_dataset %>%
  mutate(Firm_ID = as.numeric(factor(Firm)))
drug_id <- panel_dataset %>% select(Drug_ID, Name) %>% distinct()
firm_id <- panel_dataset %>% select(Firm_ID, Firm) %>% distinct()

################################################
#read all excel files in the folder
file_list <- list.files(path = "~/Desktop/Data/drug_data/excel", pattern = "*.xlsx", full.names = TRUE)
data_list <- lapply(file_list, read_excel)
all_drug <- bind_rows(data_list)
# merge all drug datasets

# #rename columns in all_drug
old <- c("规范名称","规格","转化系数","最小制剂单位价格（元）","价格（元）","生产企业","公布时间", "省份")
new <- c("Name","Strength","Count","unit_price","Price","Firm","Date","Area")

# keep only those old names that actually exist in the data
idx <- old %in% names(all_drug)
old2 <- old[idx]
new2 <- new[idx]

# build rename call programmatically
syms_old <- rlang::syms(old2)
names(syms_old) <- new2

all_drug <- dplyr::rename(all_drug, !!!syms_old)

all_drug <- all_drug %>% select(Name, Strength, Count, unit_price, Price, Firm, Date, Area)
all_drug$Date <- as.Date(all_drug$Date)
all_drug$Year <- year(all_drug$Date)
all_drug <- all_drug %>% select(-Date)

panel_dataset$is_treated <- 0
panel_dataset$is_treated <- ifelse(panel_dataset$Year > 2017, 1, 0)

all_drug$is_treated <- 0
#match firm and drug id in all_drug
all_drug <- left_join(all_drug, drug_id, by = "Name", relationship = "many-to-one")
all_drug <- left_join(all_drug, firm_id, by = "Firm", relationship = "many-to-one")

# merge panel_both with all_drug
names( all_drug )
names( panel_both )
# ensure the columns are in the same order
all_drug <- all_drug %>% select(Name, Firm, Price, Strength, Count, unit_price, Year, Area, Drug_ID, Firm_ID, is_treated)
names( all_drug )
new_panel <- rbind(panel_dataset, all_drug)


new_panel <- new_panel %>%
  arrange(Drug_ID, Year) %>%
  group_by(Drug_ID) %>%
  mutate(is_treated = ifelse(cummax(is_treated) > 0, 1, 0)) %>%
  ungroup()

#test
#select the list of drug name from all_drug
drug <- unique(all_drug$Name)
test <- new_panel %>% filter(Name %in% drug)
test <- test %>% filter(!is.na(Drug_ID))

#calculate the average price by drug id and year
test$unit_price <- as.numeric(gsub("[^0-9\\.]", "", test$unit_price))


write.csv(test, "~/Desktop/Data/test_did.csv", row.names = FALSE)


data <- test

# # Clean price and identify treatment cohort for Sun & Abraham estimator
# data <- data %>%
#   mutate(
#     Year = as.integer(Year),
#     unit_price = as.numeric(gsub("[^0-9\\.]", "", unit_price))
#   ) %>%
#   filter(!is.na(unit_price)) %>%
#   group_by(Name) %>%
#   mutate(
#     first_treat_year = ifelse(any(is_treated == 1), min(Year[is_treated == 1]), 0)
#   ) %>%
#   ungroup()
# write.csv(data, "~/Desktop/Data/cleaned_data.csv", row.names = FALSE)

