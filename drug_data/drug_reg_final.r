library(dplyr)
library(tidyr)
library(did)
library(ggplot2)
library(fixest)
library(zoo)
# data preparation
read.csv("~/Desktop/Data/test_did.csv") -> data
########################################
#fixed effect model without handling the missing prices
#average price of each drug before and after VBP

data <- data %>%
  filter(unit_price <= 1000)
avg_price <- data %>%
  group_by(Drug_ID, is_treated, Strength) %>%
  summarise(avg_price = mean(unit_price, na.rm = TRUE)) %>%
  ungroup()

strength_counts <- avg_price %>%
  group_by(Drug_ID, Strength) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n == 2)

avg_price <- avg_price %>%
  inner_join(strength_counts, by = c("Drug_ID", "Strength")) %>%
  select(-n)

#drop if avg_price is NA
avg_price <- avg_price %>% filter(!is.na(avg_price))
sink("~/Desktop/Data/drug_output_log.txt", append = TRUE)
#merge the first treatment year to avg_price
first_treat_year <- data %>%
  group_by(Drug_ID) %>%
  summarise(first_treat_year = ifelse(any(is_treated == 1), min(Year[is_treated == 1]), NA_integer_), .groups = "drop")

avg_price <- avg_price %>%
  left_join(first_treat_year, by = "Drug_ID") %>%
  filter(!is.na(first_treat_year))

ols_fe <- feols(
  avg_price ~ is_treated | Drug_ID + Strength + first_treat_year,
  cluster = ~Drug_ID,
  data = avg_price
)
print("Fixed Effect Model without Handling Missing Prices:")
print("Regression euqation: avg_price ~ is_treated | Drug_ID + Strength + first_treat_year")
print("Results:")
summary(ols_fe)


#export the avg_price data
write.csv(avg_price, "~/Desktop/Data/avg_price.csv", row.names = FALSE)
########################################
#fixed effect model with handling the missing prices
#average price of each drug before and after VBP
#remove dataset that is not used in the following analysis
rm(drug_id, firm_id, panel_both)
read.csv("~/Desktop/Data/test_did.csv") -> data

drug_panel <- data %>%
  mutate(
    Year       = as.integer(gsub("\\D","", as.character(Year))),
    unit_price = suppressWarnings(as.numeric(gsub("[^0-9\\.]", "", as.character(unit_price))))
  ) %>%
  filter(!is.na(Year), !is.na(unit_price)) %>%
  group_by(Drug_ID, Year) %>%
  summarise(unit_price = log(mean(unit_price, na.rm = TRUE)), .groups = "drop")


#
g_drug <- data %>%
  mutate(Year = as.integer(gsub("\\D","", as.character(Year)))) %>%
  group_by(Drug_ID) %>%
  summarise(
    g = if (any(is_treated == 1, na.rm = TRUE))
          min(Year[is_treated == 1], na.rm = TRUE) else NA_integer_,
    .groups = "drop"
  )

drug_panel <- drug_panel %>%
  left_join(g_drug, by = "Drug_ID") %>%
  filter(!is.na(g)) %>%
  mutate(Drug_ID_num = as.integer(factor(Drug_ID)))
drug_panel <- drug_panel %>%
  arrange(Drug_ID_num, Year) %>%
  select(Drug_ID_num, Year, unit_price, g, everything())

drug_panel <- drug_panel %>%
  mutate(treated = ifelse(Year >= g, 1, 0))
########################################

model1 <- feols(
  unit_price ~ treated | Drug_ID_num + Year,
  cluster = ~Drug_ID_num,
  data = drug_panel
)
summary(model1)




#missing values handling
#for each drug_ID, add missing years and fill the missing prices with 0
drug_panel <- drug_panel %>%
  group_by(Drug_ID_num) %>%
  complete(Year = full_seq(min(Year):max(Year), 1)) %>%
  arrange(Drug_ID_num, Year) %>%
  mutate(unit_price = ifelse(is.na(unit_price), NA, unit_price)) %>%
  fill(g, .direction = "downup") %>%
  select(-Drug_ID) %>%
  ungroup()

#fill treated == ifelse(Year >= g, 1, 0)
drug_panel <- drug_panel %>%
  mutate(treated = ifelse(Year >= g, 1, 0))

write.csv(drug_panel, "~/Desktop/Data/drug_panel.csv", row.names = FALSE)
drug_panel %>%
  summarise(
    total_obs = n(),
    missing_prices = sum(is.na(unit_price)),
    pct_missing = mean(is.na(unit_price)) * 100
  )

drug_panel %>%
  group_by(Year) %>%
  summarise(pct_missing = mean(is.na(unit_price)) * 100)

#linear interpolation to fill the missing prices
drug_panel_li <- drug_panel %>%
  group_by(Drug_ID_num) %>%
  arrange(Year) %>%
  mutate(unit_price = na.approx(unit_price, Year, na.rm = FALSE)) %>%
  ungroup()

print("linear interpolation to fill the missing prices:")
fe_simple_li <- feols(
  unit_price ~ treated | Drug_ID_num + Year,
  cluster = ~Drug_ID_num,
  data = drug_panel_li
)
summary(fe_simple_li)
esttex(
  fe_simple_li,
  file = "~/Desktop/Data/drug_data/fe_simple_li.tex",
  replace = TRUE,
  title = "FE on log price (linear interpolation fill)"
)
#use previous years' price to fill the missing prices
drug_panel_pl <- drug_panel %>%
  group_by(Drug_ID_num) %>%
  arrange(Year) %>%
  fill(unit_price, .direction = "downup") %>%
  ungroup()
drug_panel_pl <- drug_panel_pl %>%
  mutate(
    event_time = Year - g
  )

fe_simple_pl <- feols(
  unit_price ~ treated | Drug_ID_num + Year,
  cluster = ~Drug_ID_num,
  data = drug_panel_pl
)

fe_event <- feols(
  unit_price ~ i(event_time, ref = -1) | Drug_ID_num + Year,
  cluster = "Drug_ID_num",
  data = drug_panel_pl
)
summary(fe_event)

event_df <- broom::tidy(fe_event, conf.int = TRUE) %>%
  filter(grepl("event_time::", term)) %>%
  mutate(
    event_time = as.numeric(gsub("event_time::", "", term))
  ) %>%
  arrange(event_time)
event_plot <- ggplot(event_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = 0, linetype = "solid", color = "red", alpha = 0.5) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
  labs(
    x = "Years relative to treatment",
    y = "Effect on log unit price",
    title = "Event Study: Carry-Forward/Backfilled Prices"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  filename = "~/Desktop/Data/drug_data/fe_event_plot.png",
  plot = event_plot,
  width = 7,
  height = 5,
  dpi = 300
)


esttex(
  fe_event,
  file = "~/Desktop/Data/drug_data/drug_model.tex",
  replace = TRUE,
x  title = "Event study on log unit price (carry-forward price fill)"
)
########################################
#clean the data and make it balanced panel
drug_panel_2018 <- drug_panel %>% filter(g == 2018)
drug_panel_2020 <- drug_panel %>% filter(g == 2020)
drug_panel_2021 <- drug_panel %>% filter(g == 2021)

  
all_years <- seq(2015, 2020, by = 1)
drug_panel_2018 <- drug_panel_2018 %>%
  group_by(Drug_ID_num) %>%
  complete(Year = all_years) %>%
  arrange(Drug_ID_num, Year) %>%
  mutate(unit_price = zoo::na.approx(unit_price, na.rm = FALSE)) %>% 
  ungroup()

drug_panel_2018 <- drug_panel_2018 %>%
  group_by(Drug_ID_num) %>%
  fill(unit_price, .direction = "downup") %>%
  fill(g, .direction = "downup") %>%
  ungroup()

all_years <- seq(2018, 2022, by = 1)
drug_panel_2020 <- drug_panel_2020 %>%
  group_by(Drug_ID_num) %>%
  complete(Year = all_years) %>%
  arrange(Drug_ID_num, Year) %>%
  mutate(unit_price = zoo::na.approx(unit_price, na.rm = FALSE)) %>%  # linear interpolation
  ungroup()

drug_panel_2020 <- drug_panel_2020 %>%
  group_by(Drug_ID_num) %>%
  fill(unit_price, .direction = "downup") %>%
  fill(g, .direction = "downup") %>%
  ungroup()

all_years <- seq(2019, 2023, by = 1)
drug_panel_2021 <- drug_panel_2021 %>%
  group_by(Drug_ID_num) %>%
  complete(Year = all_years) %>%
  arrange(Drug_ID_num, Year) %>%
  mutate(unit_price = zoo::na.approx(unit_price, na.rm = FALSE)) %>%  # linear interpolation
  ungroup()
drug_panel_2021 <- drug_panel_2021 %>%
  group_by(Drug_ID_num) %>%
  fill(unit_price, .direction = "downup") %>%
  fill(g, .direction = "downup") %>%
  ungroup()

#combine the three dataframes
drug_panel <- bind_rows(drug_panel_2018, drug_panel_2020, drug_panel_2021)
# # drug_panel_bal <- drug_panel %>%
#   group_by(Drug_ID_num) %>%
#   complete(Year = all_years) %>%
#   arrange(Drug_ID_num, Year) %>%
#   mutate(unit_price = zoo::na.approx(unit_price, na.rm = FALSE)) %>%  # linear interpolation
#   ungroup()

#fill the missing values of g according to the same Drug_ID_num


#put the columns of unit_price 

print(table(drug_panel$g))

########################################



# 3) Callaway–Sant’Anna
cs <- att_gt(
  yname         = "unit_price",
  tname         = "Year",
  idname        = "Drug_ID_num",
  gname         = "g",
  data          = drug_panel,
  control_group = "notyettreated",
  est_method    = "dr",
  clustervars   = "Drug_ID_num",
  panel         = FALSE
)
length(unique(cs$DIDparams$data$Drug_ID_num))
# 4) 聚合/展示
summary(aggte(cs, type = "dynamic"))
summary(aggte(cs, type = "simple"))

table(cs$g)
es_out <- aggte(cs, type = "dynamic")

# Extract the event-study results into a dataframe
es_df <- data.frame(
  event_time = es_out$egt,             # relative time to treatment
  estimate   = es_out$att.egt,         # ATT estimate
  std_error  = es_out$se.egt,          # standard error
  lower      = es_out$att.egt - 1.96 * es_out$se.egt,
  upper      = es_out$att.egt + 1.96 * es_out$se.egt
)

ggplot(es_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(size = 2, color = "blue") +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  labs(
    title = "Event-Study Estimates (Dynamic ATT, Callaway–Sant’Anna)",
    x = "Event Time (years relative to treatment)",
    y = "ATT Estimate"
  ) +
  theme_minimal()

sink()  
##################
library(fixest)

# Sun & Abraham estimator
sa_mod <- feols(
  unit_price ~ sunab(g, Year) | Drug_ID_num + Year,
  cluster = ~Drug_ID_num,
  data = drug_panel
)

summary(sa_mod)
# see the observations in each cohort
drug_panel %>%
  mutate(event_time = Year - g) %>%
  group_by(event_time) %>%
  summarise(n = n())

drug_panel %>%
  group_by(Year) %>%
  summarise(n = n())
# Plot the event-study coefficients
# iplot(sa_mod, ref.line = 0, main = "Event Study (Sun & Abraham)")

# Summary of results
summary(res)

res_sunab_bin = feols(
      unit_price ~ sunab(g, Year, bin.p = c(2015, 2016, 2023, 2024)) | Drug_ID_num + Year,
      cluster = ~Drug_ID_num,
      data = drug_panel
    )

summary(res_sunab_bin)


iplot(res_sunab_bin, ref.line = -1, main = "Event-study with binned tails")

sink() 
