
#categorize firms into large and small based on it's data in 2017 median RegisterCapital in year 2017
firm_data <- read.csv("~/Desktop/Data/firm_data/firm_reg_data.csv")
firm_2017 <- firm_data %>%
  filter(Year == 2017) %>%
  select(Symbol, RegisterCapital) %>%
  rename(RegisterCapital_2017 = RegisterCapital)

#if median RegisterCapital_2017 is NA, remove those firms from consideration
firm_2017 <- firm_2017 %>%
  filter(!is.na(RegisterCapital_2017))
median_cap_2017 <- median(firm_2017$RegisterCapital_2017, na.rm = TRUE)
median_cap_2017

firm_2017 <- firm_2017 %>%
  mutate(size_group = ifelse(RegisterCapital_2017 >= median_cap_2017,
                             "Large", "Small"))


firm_data <- firm_data %>%
  left_join(firm_2017 %>% select(Symbol, size_group),
            by = "Symbol",
            relationship = "many-to-many")

#change size_group NA to different category according to RegisterCapital in that year
firm_data <- firm_data %>%
  mutate(size_group = ifelse(is.na(size_group) & !is.na(RegisterCapital),
                             ifelse(RegisterCapital >= median_cap_2017,
                                    "Large", "Small"),
                             size_group))

#drop rows with size_group still NA
firm_data <- firm_data %>%
  filter(!is.na(size_group))



#event study setup
#find the first win year for each winning firm
first_win <- firm_data %>%
  filter(win_y == 1) %>%
  group_by(Symbol) %>%
  summarise(first_win_year = min(Year)) %>%
  ungroup()
firm_data <- firm_data %>%
  left_join(first_win, by = "Symbol")

firm_data <- firm_data %>%
  mutate(
    Year = as.numeric(Year),
    first_win_year = as.numeric(first_win_year),
    event_time = ifelse(is.na(first_win_year),
                        NA,
                        Year - first_win_year)
  )

for (k in -4:4) {
  if (k != -1) {
    varname <- paste0("event_", k)
    firm_data[[varname]] <- as.numeric(firm_data$event_time == k)
    firm_data[[varname]][is.na(firm_data$event_time)] <- 0   # non-winners get 0
  }
}
firm_level <- firm_data %>%
  group_by(Symbol) %>%
  summarise(ever_win = max(win, na.rm = TRUE))

firm_data <- firm_data %>%
  left_join(firm_level, by = "Symbol")

mean_year_trends <- firm_data %>%
  filter(Year >= 2014, Year <= 2022) %>%
  group_by(size_group, ever_win, Year) %>%
  summarise(
    mean_RD = mean(RDSpendSum, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    win_status = ifelse(ever_win == 1, "Winner", "Non-winner"),
    group_label = paste(size_group, win_status, sep = "_")
  )




#label the firm as large_winner and small_winner for year before 2018 consistent with the year later





library(ggplot2)

ggplot(mean_year_trends,
                  aes(x = Year, y = mean_RD,
                      color = group_label, linetype = group_label)) +

  geom_line(size = 1.2) +
  geom_point(size = 2) +

  geom_vline(xintercept = 2018, color = "red", alpha = 0.4) +

  scale_x_continuous(breaks = 2014:2022) +

  labs(
    x = "Year",
    y = "Mean R&D Spending",
    color = "Firm Group",
    linetype = "Firm Group",
    title = "R&D Spending Trajectories, 2014–2022\nBy Firm Size and NVBP Winning Status"
  ) +
  theme_minimal(base_size = 14)

firm_data <- firm_data %>%
  mutate(
    post = ifelse(Year >= 2018, 1, 0),
    Small = ifelse(size_group == "Small", 1, 0)
  )

#save the plot
ggsave(
  filename = "~/Desktop/Data/firm_data/rd_spending_trajectories_by_size_and_winning_status.png",
  width = 10,
  height = 6
)

did_size <- feols(
  RDSpendSum ~ win * post * Small | Symbol + Year,
  cluster = "Symbol",
  data = firm_data
)

summary(did_size)



firm_data <- firm_data %>%
  mutate(
    group4 = case_when(
      win==1 & size_group=="Large" ~ "Large_Winner",
      win==0 & size_group=="Large" ~ "Large_Nonwinner",
      win==1 & size_group=="Small" ~ "Small_Winner",
      win==0 & size_group=="Small" ~ "Small_Nonwinner"
    )
  )

did4 <- feols(
  RDSpendSum ~ i(group4) | Symbol + Year,
  cluster = "Symbol",
  data = firm_data
)
summary(did4)
