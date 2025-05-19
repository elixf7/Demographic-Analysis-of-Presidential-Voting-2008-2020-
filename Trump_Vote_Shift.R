###############################################################################
# County-Level Trump Vote Share & Demographics
#
# This script:
#   1. Loads county-level presidential vote data (2000-2020) and ACS 5-year
#      demographic estimates (2009-2020).
#   2. Calculates Trump’s county vote-share for 2016 and 2020.
#   3. Flags counties with unusually large changes in Trump support.
#   4. Merges the “big-shift” counties with 2020 demographic percentages.
#   5. Produces descriptive statistics and Welch t-tests comparing the
#      demographics of counties with large positive vs. large negative shifts.
###############################################################################

# ── 1  Load packages ──────────────────────────────────────────────────────────
library(tidyverse)
library(janitor)   # clean_names()
library(broom)     # tidy() for model/test output
library(stringr)   # str_pad()

# ── 2  House-keeping ──────────────────────────────────────────────────────────
rm(list = ls())
setwd("/Users/elifried/Desktop/Poli281/Final Project/Data-Final")

# ── 3  Read data ──────────────────────────────────────────────────────────────
pres <- read_csv("countypres_2000-2020.csv")   # MIT Election data
acs  <- read_csv("acs-2009-2020.csv")          # ACS 5-year estimates

# ── 4  Trump vote-share (2016 & 2020) ─────────────────────────────────────────
trump_share <- pres %>%
  filter(
    year   %in% c(2016, 2020),
    office == "US PRESIDENT",
    mode   == "TOTAL"               # keep only “TOTAL” rows
  ) %>%
  group_by(year, state, county_name, county_fips) %>%
  summarise(
    trump_votes = sum(candidatevotes[party == "REPUBLICAN"], na.rm = TRUE),
    total_votes = first(totalvotes),
    .groups     = "drop"
  ) %>%
  mutate(trump_share = trump_votes / total_votes) %>%
  select(year, state, county_name, county_fips, trump_share)

share_wide <- trump_share %>%
  pivot_wider(
    names_from  = year,
    values_from = trump_share,
    names_glue  = "trump_share_{year}"
  )

# ── 5  Identify large shifts in support ───────────────────────────────────────
shifts <- share_wide %>%
  mutate(
    delta_share = trump_share_2020 - trump_share_2016,
    z_delta     = as.numeric(scale(delta_share))
  ) %>%
  arrange(desc(abs(z_delta)))        # largest absolute changes first

# Counties with |z| ≥ 2
significant <- shifts %>% filter(abs(z_delta) >= 2)
# print(significant, n = 80)

# ── 6  Prepare demographic data & shift labels ───────────────────────────────
z_thresh <- 1   # threshold for “big” shift (≥1 SD)

shifts_grp <- shifts %>%
  mutate(group = case_when(
    z_delta >=  z_thresh ~ "PositiveShift",
    z_delta <= -z_thresh ~ "NegativeShift",
    TRUE                 ~ NA_character_
  )) %>%
  filter(!is.na(group)) %>%
  mutate(county_fips = str_pad(county_fips, 5, pad = "0")) %>%
  select(county_fips, group)

acs_pct <- acs %>%
  filter(end_year == 2020) %>%        # use latest ACS 5-year period
  select(GEOID, starts_with("pct_")) %>%
  mutate(GEOID = str_pad(as.character(GEOID), 5, pad = "0"))

dat <- left_join(shifts_grp, acs_pct,
                 by = c("county_fips" = "GEOID")) %>%
  drop_na()                           # drop counties missing either side
glimpse(dat)

# ── 7  Descriptive summary ───────────────────────────────────────────────────
summary_tbl <- dat %>%
  pivot_longer(starts_with("pct_"), names_to = "var", values_to = "value") %>%
  group_by(group, var) %>%
  summarise(
    mean   = mean(value,   na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    n      = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from  = group,
    values_from = c(mean, median, n),
    names_sep   = "_"
  )

# ── 8  Welch t-tests between shift groups ────────────────────────────────────
ttest_tbl <- dat %>%
  pivot_longer(starts_with("pct_"), names_to = "var", values_to = "value") %>%
  group_by(var) %>%
  nest() %>%
  mutate(
    tinfo = map(
      data,
      ~ t.test(value ~ group, data = .x, var.equal = FALSE) %>% tidy()
    )
  ) %>%
  unnest(tinfo) %>%
  select(
    var,
    mean_neg = estimate1,   # broom orders factor levels alphabetically
    mean_pos = estimate2,
    t_stat   = statistic,
    p_value  = p.value
  ) %>%
  arrange(p_value)

# ── 9  Output ────────────────────────────────────────────────────────────────
print(summary_tbl, n = Inf, width = Inf)
print(ttest_tbl,   n = Inf, width = Inf)

# Write to CSV for further inspection
# write_csv(summary_tbl, "trump_demographic_summary_by_shift.csv")
# write_csv(ttest_tbl,   "trump_demographic_ttests_by_shift.csv")

# If the CSVs already exist they can be read back in for viewing:
# trump_dem_summary <- read.csv("trump_demographic_summary_by_shift.csv")
# trump_ttest       <- read.csv("trump_demographic_ttests_by_shift.csv")
# 
# trump_dem_summary
# trump_ttest
