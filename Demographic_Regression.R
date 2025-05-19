###############################################################################
# County-Level Republican Vote-Share Modelling (2012-2020)
#
# • Fits a separate linear model for each election year (2012, 2016, 2020).
# • Predictors: county-level demographic percentages (ACS) + log(population).
# • Outputs:
#     – fit_stats   : R-squared, RMSE, N for each year
#     – coef_tbl    : tidy table of standardised coefficients
#     – coef_wide   : wide version of coefficients (optional)
############################################################################### :contentReference[oaicite:0]{index=0}:contentReference[oaicite:1]{index=1}

# ── 1  Load packages ──────────────────────────────────────────────────────────
library(tidyverse)
library(broom)      # tidy() for model output
library(stringr)    # str_pad()

# ── 2  Read data & harmonise FIPS codes ──────────────────────────────────────
setwd("/Users/elifried/Desktop/Poli281/Final Project/Data-Final")

acs  <- read_csv("acs-2009-2020.csv")   %>% mutate(GEOID       = str_pad(GEOID,  5, pad = "0"))
pres <- read_csv("countypres_2000-2020.csv") %>% mutate(county_fips = str_pad(county_fips, 5, pad = "0"))

# ── 3  Build county-level two-party vote shares ──────────────────────────────
rep_shares <- pres %>%
  mutate(party = str_to_upper(party)) %>%                # harmonise casing
  filter(party %in% c("REPUBLICAN", "DEMOCRAT")) %>%     # keep the two majors
  group_by(year, county_fips) %>%
  summarise(
    county_name = paste0(
      str_to_sentence(str_to_lower(first(county_name))), # e.g. "Autauga"
      ", ",
      first(state_po)                                    # e.g. "AL"
    ),
    rep_votes  = sum(candidatevotes[party == "REPUBLICAN"], na.rm = TRUE),
    dem_votes  = sum(candidatevotes[party == "DEMOCRAT"],   na.rm = TRUE),
    two_party  = rep_votes + dem_votes,
    rep_share  = rep_votes / two_party,
    dem_share  = dem_votes / two_party,
    .groups    = "drop"
  ) %>%
  select(year, county_fips, county_name, rep_share, dem_share)

# ── 4  Prepare ACS predictors (percent variables + total population) ─────────
acs_pct <- acs %>%
  filter(end_year %in% c(2009, 2012, 2016, 2020)) %>%
  mutate(end_year = if_else(end_year == 2009, 2008, end_year)) %>%  # 2009 ACS ≈ 2008 election
  select(
    year      = end_year,
    GEOID,
    total_pop = total_population,     # rename for clarity
    starts_with("pct_")
  ) %>%
  select(-pct_asian, -pct_under_18, -pct_some_college)   # drop collinear vars

# ── 5  Merge vote shares with demographics ───────────────────────────────────
dat <- left_join(
  rep_shares,
  acs_pct,
  by = c("county_fips" = "GEOID", "year")
) %>%
  drop_na() %>%                                 # remove incomplete rows
  select(
    county_fips,
    county_name,
    year,
    total_pop,
    rep_share,
    starts_with("pct_")
  )

# ── 6  Modelling helper function ─────────────────────────────────────────────
fit_one_year <- function(df) {
  yr <- unique(df$year)
  
  # log-transform population
  df <- df %>% mutate(log_pop = log1p(total_pop))
  
  # predictor names
  pct_vars <- names(df)[startsWith(names(df), "pct_")]
  preds    <- c("log_pop", pct_vars)
  
  # standardise predictors (mean 0, sd 1)
  df[preds] <- lapply(df[preds], \(x) as.numeric(scale(x)))
  
  # fit model: rep_share ~ predictors
  mod <- lm(reformulate(preds, response = "rep_share"), data = df)
  
  # tidy output
  tibble(
    year   = yr,
    adj_r2 = summary(mod)$adj.r.squared,
    rmse   = sqrt(mean(mod$residuals^2)),
    n      = nrow(df),
    coefs  = list(tidy(mod) %>% filter(term != "(Intercept)"))
  )
}

# ── 7  Run models (2012, 2016, 2020) ─────────────────────────────────────────
results <- dat %>%
  group_by(year) %>%
  group_modify(~ fit_one_year(.x))

# ── 8  Unpack results ────────────────────────────────────────────────────────
fit_stats <- results %>% select(year, adj_r2, rmse, n)

coef_tbl  <- results %>%
  select(year, coefs) %>%
  unnest(coefs)

# Optional: wide layout for quick comparison
coef_wide <- coef_tbl %>%
  select(year, term, estimate) %>%
  pivot_wider(names_from = year, values_from = estimate)

# ── 9  Inspect results ───────────────────────────────────────────────────────
print(fit_stats)

# Top 10 absolute coefficients per year
coef_tbl %>%
  group_by(year) %>%
  slice_max(order_by = abs(estimate), n = 10) %>%
  arrange(year, desc(abs(estimate))) %>%
  print(n = Inf, width = Inf)

# ── 11  Launch Shiny app (optional) ──────────────────────────────────────────
setwd("/Users/elifried/Desktop/Poli281/Final Project/App_files")
shiny::runApp()

# library(rsconnect)
# setwd("/Users/elifried/Desktop/Poli281/Final Project/App_files")
# rsconnect::setAccountInfo(name='efried',
#                           token='F4ECADCFB8C8FFE15D63ED4AD246D8EB',
#                           secret='vXghC4wCMBF9uQCxpzSpLJryOsNLyfgUStpltFqK')
# rsconnect::deployApp()
