# Load required library
library(dplyr)
library(ggplot2)
library(broom)
library(readr)
library(scales)

#get the pharm firm code
Pharma_Code <- IPO_code[(IPO_code$IndustryCode %in% "C27"),]
Pharma_Code <- Pharma_Code [!duplicated(Pharma_Code$StockCode), ]


#select the pharma firm investment


# Read your files
pt_num <- read.csv("~/Desktop/Data/firm_data/PT_num.csv", stringsAsFactors = FALSE)
# rd_invst <- read.csv("~/Desktop/Data/firm_data/RD_invst.csv", stringsAsFactors = FALSE)
rd_invst <- read.csv("~/Desktop/Data/firm_data/PT_PharmaSPENDING.csv", stringsAsFactors = FALSE)
colnames(rd_invst)[2] <- "StockCode"
staff <- read_excel("~/Desktop/Data/firm_data/CompanyStaff.xlsx")
staff$StockCode <- as.integer(staff$StockCode)

# Extract year from EndDate.x
pt_num$year <- format(as.Date(pt_num$EndDate.x), "%Y")
rd_invst$year <- format(as.Date(rd_invst$EndDate), "%Y")
rd_invst <- rd_invst[as.numeric(rd_invst$year) > 2010, ]
staff$year <- format(as.Date(staff$EndDate), "%Y")

rd_invst <- rd_invst %>%left_join(staff, by = c("StockCode", "year"),relationship = "many-to-many")
# keep all observations from both datasets

rd_invst <- rd_invst %>%
  filter(year != 2024)

# Keep only needed columns
pt_num_sel <- pt_num %>%
  select(StockCode, year, Patents, ShortName)

rd_invst_sel <- rd_invst %>%
  select(StockCode, year, RDSpendSum,RDSpendSumRatio,RDPerson, RDPersonRatio, Amount )

write.csv(rd_invst_sel, "~/Desktop/Data/firm_data/rd_invst_sel.csv", row.names = FALSE)
# Merge datasets
firm_panel_data <- merge(pt_num_sel, rd_invst_sel,
                    by = c("StockCode", "year"),
                    all = TRUE)

# Sort by StockCode and year
firm_panel_data <- panel_data %>%
  arrange(StockCode, year)

# View first few rows
head(panel_data)
######################################
write.csv(panel_data, "~/Desktop/Data/firm_data/firm_panel_dataset.csv", row.names = FALSE)

#categorize firm based on firm size
rd_invst_sel <- rd_invst_sel %>%
  filter(!is.na(Amount)) %>%
  mutate(staff_category = case_when(
    Amount < 1000 ~ "<1000",
    Amount <= 5000 ~ "1000-5000",
    Amount > 5000 ~ ">5000"
  ),
  post = ifelse(year >= 2018, 1, 0))

######################################
# Calculate average RDSpendSum per year for each staff category
avg_invest_by_cat <- rd_invst_sel %>%
  group_by(year, staff_category) %>%
  summarise(avg_RDSpendSum = mean(RDSpendSum, na.rm = TRUE)) %>%
  ungroup()
# Calculate average RDSpendSum per year overall
avg_invest_total <- rd_invst_sel %>%
  group_by(year) %>%
  summarise(avg_RDSpendSum = mean(RDSpendSum, na.rm = TRUE)) %>%
  mutate(staff_category = "All Firms") %>%
  ungroup()

# Calculate average RDSpendSumRatio per year for each staff category
avg_investRatio_by_cat <- rd_invst_sel %>%
  group_by(year, staff_category) %>%
  summarise(avg_RDSpendRatio = mean(RDSpendSumRatio, na.rm = TRUE)) %>%
  ungroup()
# Calculate average RDSpendSum per year overall
avg_investRatio_total <- rd_invst_sel %>%
  group_by(year) %>%
  summarise(avg_RDSpendRatio = mean(RDSpendSumRatio, na.rm = TRUE)) %>%
  mutate(staff_category = "All Firms") %>%
  ungroup()


#################################################
# Plot
#################################################
avg_invest_plot <- bind_rows(avg_invest_by_cat, avg_invest_total)
avg_invest_ratio_plot <- bind_rows(avg_investRatio_by_cat, avg_investRatio_total)

# Plot - SpendSum
ggplot(avg_invest_plot, aes(x = as.numeric(year), y = avg_RDSpendSum, color = staff_category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(min(avg_invest_plot$year), max(avg_invest_plot$year), 1)) +
  labs(
    title = "Average R&D Investment per Year by Staff Size Category and Overall",
    x = "Year",
    y = "Average RDSpendSum",
    color = "Group"
  ) +
  theme_minimal()

# Plot - SpendSumRatio
ggplot(avg_invest_ratio_plot, aes(x = as.numeric(year), y = avg_RDSpendRatio, color = staff_category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(min(avg_invest_plot$year), max(avg_invest_plot$year), 1)) +
  labs(
    title = "Average R&D Investment Ratio per Year by Staff Size Category and Overall",
    x = "Year",
    y = "Average RDSpendSum",
    color = "Group"
  ) +
  theme_minimal()
#
rd_invst_sel$staff_category <- factor(rd_invst_sel$staff_category, 
                                      levels = c("<1000", "1000-5000", ">5000"))

# DID regression
did_model <- lm(RDSpendSum ~ staff_category * post, data = rd_invst_sel)

# Summarize results
summary(did_model)

# Tidy table of coefficients
tidy(did_model)

#################################################
#Summary: number of firms per category per year
firm_count_summary <- rd_invst_sel %>%
  group_by(year, staff_category) %>%
  summarise(n_firms = n_distinct(StockCode)) %>%
  arrange(year, staff_category)

write.csv(firm_count_summary, "~/Desktop/Data/firm_data/firm_count.csv", row.names = FALSE)


firm_count <- read.csv("~/Desktop/Data/firm_data/firm_count.csv") %>%
  mutate(year = as.numeric(year))

ratio_tbl <- firm_count %>%
  group_by(year) %>%
  mutate(total_year = sum(n_firms),
         ratio = n_firms / total_year) %>%
  ungroup()

# 100% stacked bars
ggplot(ratio_tbl, aes(x = year, y = ratio, fill = staff_category)) +
  geom_col(position = "fill") +
  geom_text(aes(label = paste0( n_firms )),
            position = position_fill(vjust = 0.5), size = 3, color = "white") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(breaks = seq(min(ratio_tbl$year), max(ratio_tbl$year), 1)) +
  labs(title = "Share of Firms by Size Group Each Year",
       x = "Year", y = "Share of Firms", fill = "Size Group") +
  theme_minimal()

# Line chart
ggplot(ratio_tbl, aes(x = year, y = ratio, color = staff_category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(breaks = seq(min(ratio_tbl$year), max(ratio_tbl$year), 1)) +
  labs(title = "Firm Size Composition Over Time",
       x = "Year", y = "Share of Firms", color = "Size Group") +
  theme_minimal()


print(firm_count_summary)
######################################################
# library(readxl)
# PT_LCRDSPENDING <- read_excel("Desktop/Data/IPO_code/PT_LCRDSPENDING.xlsx")
# colnames(PT_LCRDSPENDING)[1] <- "StockCode"
# rd_all<- merge(
#   PT_LCRDSPENDING,
#   IPO_code[, c("StockCode", "IndustryCode")],
#   by = "StockCode",
#   all.x = TRUE
# ) #TBC
# 
# rd_all <- rd_all [!duplicated(rd_all), ]
# 
# # View result
# head(merged_data)