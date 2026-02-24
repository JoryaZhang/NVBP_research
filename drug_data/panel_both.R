# =============================
# Callaway–Sant'Anna DiD (staggered cohorts)
# Dataset: panel_both.csv  (cols: Drug_ID, Year, unit_price, ... )
# Focus: molecule (Drug_ID) × year, ignoring geography & firm
# =============================

# ---- 0) Packages ----
need <- c("data.table","dplyr","did","ggplot2","readr","stringr")
to_install <- setdiff(need, rownames(installed.packages()))
if(length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(need, library, character.only = TRUE))

# ---- 1) Load & minimal clean ----
# Set your working directory so the path is correct, or use an absolute path
dat <- read_csv("~/Desktop/Data/drug_data/panel_both.csv",
                col_types = cols(
                  Drug_ID = col_integer(),
                  Year = col_integer(),
                  unit_price = col_double()
                ))

drug_list <- panel_both %>%
  distinct(Name)

# Keep necessary columns; drop non-positive prices (log safety)
dat <- dat %>%
  select(Drug_ID, Year, unit_price) %>%
  filter(!is.na(Drug_ID), !is.na(Year), !is.na(unit_price), unit_price > 0)

# ---- 2) Collapse to molecule-year (robust to outliers) ----
# Median unit price across all firm/province rows within Drug_ID × Year
mol_year <- dat %>%
  group_by(Drug_ID, Year) %>%
  summarize(price = median(unit_price, na.rm = TRUE), .groups = "drop") %>%
  mutate(log_price = log(price))

# ---- 3) Define staggered treatment cohorts ----
# Policy window starts in 2018; first_treat = first observed post-2017 year per molecule
POLICY_START <- 2018
first_treat_tbl <- mol_year %>%
  filter(Year >= POLICY_START) %>%
  group_by(Drug_ID) %>%
  summarize(first_treat = min(Year), .groups = "drop")

# Merge; molecules that never appear post-2017 will have NA (never treated)
panel <- mol_year %>%
  left_join(first_treat_tbl, by = "Drug_ID")

# Optional: restrict to a reasonable window
# panel <- panel %>% filter(Year %in% 2015:2021)

# ---- 4) Diagnostics (cohorts, availability) ----
message("Unique first_treat cohorts (year => #drugs):")
print(panel %>% distinct(Drug_ID, first_treat) %>%
        count(first_treat, name = "n_drugs") %>% arrange(first_treat))

# ---- 5) DID setup for 'did::att_gt' ----
# We use not-yet-treated control group, which is valid when eventually everyone is treated,
# and/or you have post years with some molecules untreated yet.
# att_gt allows unbalanced panels when panel=TRUE.

# Keep only years & ids used
est_dat <- panel %>%
  mutate(
    # did prefers clean integer id/time names
    id = Drug_ID,
    t = Year,
    g = first_treat
  ) %>%
  arrange(id, t)

# Ensure each treated unit has at least one pre-period (g-1) observed
# (att_gt will drop ineligible units anyway, but this warns you.)
pre_ok <- est_dat %>%
  filter(!is.na(g)) %>%
  group_by(id) %>%
  summarize(has_pre_at_g_minus_1 = any(t == unique(g)[1] - 1L), .groups="drop")
share_with_pre <- mean(pre_ok$has_pre_at_g_minus_1)
message(sprintf("Share of treated molecules with price observed at (g-1): %.1f%%",
                100*share_with_pre))

