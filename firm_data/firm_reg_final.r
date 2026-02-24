#firm data
rm(list=ls())
library(dplyr)
library(fixest)
library(tidyr)
library(readxl)
library(fuzzyjoin)
library(stringdist)
library(stringr)
library(ggplot2)
library(modelsummary)

read_excel("~/Desktop/Data/firm_data/PT_LCRDSPENDING.xlsx") -> firm_data
read_excel("~/Desktop/Data/firm_data/firm_name.xlsx") -> firm_name
read_excel("~/Desktop/Data/firm_data/RegisterCapital.xlsx") -> size

read.csv("~/Desktop/Data/test_did.csv") -> drug_data
sink("~/Desktop/Data/firm_output_log.txt", append = TRUE)

##########################################################
#fuzzy match the winner firm names in drug_data with firm names in R&D investment data
##########################################################
#mathc Stkcd in firm_name with Symbol in firm_dat, drop the unmatched rows
firm_data <- firm_data %>%
  left_join(firm_name, by = c("Symbol" = "Stkcd")) %>%
    filter(!is.na(Name))

firm_data <- firm_data %>%
  select(Symbol, EndDate, RDPerson, RDSpendSum, RDSpendSumRatio, RDInvest, Listdt, Name) %>%
  mutate(Year = as.numeric(format(as.Date(EndDate), "%Y")))

firm_lis <- drug_data %>%
  select(Firm_ID, Firm, Year) %>%
  distinct()

write.csv(firm_lis, "~/Desktop/Data/firm_data/firm_list.csv", row.names = FALSE)

#book contains the firm names in drug_data (bid winners)
#fuzzy match firm names
book <- read_excel("~/Desktop/Data/firm_data/Book.xlsx")

book <- book %>%
  mutate(Firm = tolower(trimws(Firm)))


firm_data <- firm_data %>%
  mutate(Name = tolower(trimws(Name)))

book <- book %>%
  rowwise() %>%
  mutate(
    substring_match = paste(
      firm_data$Name[str_detect(firm_data$Name, fixed(Firm))],
      collapse = "; "
    )
  ) %>%
  ungroup()




bid_winner <- book %>%
  filter(substring_match != "") %>%
  separate_rows(substring_match, sep = "; ") %>%
  select(Name = substring_match, Book_Name = Firm, win_Year = Year) %>%
  distinct()

#match Book_Name in book with Name, and win_Year to Year in firm_data
firm_data <- firm_data %>%
  left_join(bid_winner, by = c("Name" = "Name", "Year" = "win_Year"))

#count how many firms in firm_data and how many winner firms
total_firms <- n_distinct(firm_data$Symbol)
winner_firms <- n_distinct(firm_data$Symbol[!is.na(firm_data$Book_Name)])
print(paste("Total firms in R&D data:", total_firms))
print(paste("Winner firms matched in R&D data:", winner_firms))

firm_data$win_y <- 0

#win_y = 1 if the firm won in that year, year>=2018
firm_data <- firm_data %>%
  mutate(win_y = ifelse(!is.na(Book_Name) & Year >= 2018, 1, 0))


#group by Symbol only, variable: winner = 1 as long as the firm won in any year for year>=2018
firm_data <- firm_data %>%
  group_by(Symbol) %>%
  mutate(win = ifelse(any(win_y == 1) & Year >=2018, 1, 0)) %>%
  ungroup()

#generate win group variable for each year
firm_data$win_2018 <- ifelse(firm_data$Year == 2018 & firm_data$win_y == 1, 1, 0)
firm_data$win_2019 <- ifelse(firm_data$Year == 2019 & firm_data$win_y == 1, 1, 0)
firm_data$win_2020 <- ifelse(firm_data$Year == 2020 & firm_data$win_y == 1, 1, 0)
firm_data$win_2021 <- ifelse(firm_data$Year == 2021 & firm_data$win_y == 1, 1, 0)
##########################################################
#regression analysis
##########################################################
#data cleaning
firm_data <- firm_data %>%
  filter(Year >= 2014 & Year <= 2024)

size <- size %>%
  mutate(Year = as.numeric(format(as.Date(EndDate), "%Y"))) %>%
  select(Symbol, Year, RegisterCapital)

firm_data <- firm_data %>%
  left_join(size, by = c("Symbol", "Year"))



#regression on RDSpendSum
#drop the rows with NA RDSpendSum, change it to numeric
firm_data <- firm_data %>%
  filter(!is.na(RDSpendSum))
firm_data$RDSpendSum <- as.numeric(firm_data$RDSpendSum)
firm_data$RDPerson <- as.numeric(firm_data$RDPerson)
#log transform RDSpendSum
firm_data <- firm_data %>%
  mutate(RDSpendSum = log(RDSpendSum)) %>%
  mutate(RegisterCapital = log(as.numeric(RegisterCapital)))

##########################################################
#DID regression with winner firm and year fixed effects, controlling for firm size
# first gen a new variable called win_all, which is 1 if the firm ever won NVBP for all years
firm_data <- firm_data %>%
  group_by(Symbol) %>%
  mutate(win_all = ifelse(any(win_y == 1), 1, 0)) %>%
  ungroup()
#write the did regression model y = treated + post + treated*post + controls
model1 <- feols(RDSpendSum ~ win_all*post, data = firm_data)

summary(model1)
##########################################################
#logit regression on the probability of winning NVBP based on RegisterCapital
logit_model2 <- glm(win_y ~ RegisterCapital,data = firm_data, family = binomial(link = "logit"))
summary(logit_model2)

esttex(logit_model2, title = "Logit regression on winning NVBP")
firm_data <- firm_data %>%
  mutate(
    Year = as.integer(Year),
    win_y = as.integer(win_y),
    post = ifelse(Year >= 2018, 1, 0),                # NVBP started 2018                                 # interaction term
  )
model_size <- feols(RDSpendSum ~ post + RegisterCapital | Symbol, cluster = "Symbol", data = firm_data)
summary(model_size)


#RDSpendSum regression on year fixed effects and RegisterCapital
model_all <- feols(RDSpendSum ~ i(Year) + RegisterCapital , cluster = "Symbol", data = firm_data)
summary(model_all)

print("================== post VBP RDSpendSum ================")
model_fe <- feols(RDSpendSum ~ post | Symbol + Year), cluster = "Symbol", data = firm_data)
summary(model_fe)

#export firm_data to csv
write.csv(firm_data, "~/Desktop/Data/firm_data/firm_reg_data.csv", row.names = FALSE)

#DID regression with winnner firm at that year and year fixed effects, controlling for firm size


years <- sort(unique(firm_data$Year))
coefs <- list()



model1 <- feols(RDSpendSum ~ win | Symbol + Year , cluster = "Symbol", data = firm_data)
summary(model1)
esttex(model1)







write.csv(firm_data, "~/Desktop/Data/firm_data/firm_data.csv", row.names = FALSE)
