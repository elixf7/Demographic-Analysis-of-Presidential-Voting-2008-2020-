# Demographic Analysis of Presidential Voting (2008–2020)

A county-level statistical analysis of how U.S. demographics relate to presidential
vote choice across four election cycles, paired with an interactive R Shiny app for
exploring the results.

**Live app:** https://efried.shinyapps.io/app_files/

## Overview

This project links **U.S. Census Bureau demographic data** to **county-level presidential
election returns** to answer two questions:

1. *Which demographic characteristics best predict a county's Republican vote share, and
   how has the strength of each predictor changed from 2008 to 2020?*
2. *Which counties shifted most in their support for Trump between 2016 and 2020, and how
   do those counties differ demographically?*

The analysis covers ~3,100 U.S. counties and combines API-sourced demographic features
with historical election results, modeled using standardized linear regression and
hypothesis testing.

## Data Sources

| Source | Data | Access |
| --- | --- | --- |
| U.S. Census Bureau — ACS 5-year estimates | County-level education, race/ethnicity, median income, and age cohorts for 2009, 2012, 2016, 2020 | [`tidycensus`](https://walker-data.com/tidycensus/) API wrapper |
| MIT Election Data & Science Lab | County Presidential Election Returns, 2000–2020 | Public dataset |

> The 2009 ACS 5-year period is used as the demographic baseline for the 2008 election.

## Repository Structure

```
.
├── CensusBureau-API.R        # Pulls & engineers ACS demographic features by county
├── Demographic_Regression.R  # Per-year regression of Republican vote share on demographics
├── Trump_Vote_Shift.R        # 2016→2020 vote-shift detection + demographic t-tests
├── secrets.example.R         # Credentials template — copy to secrets.R and fill in
├── Data-Final/               # Cached ACS + election CSVs
└── App_files/                # Interactive R Shiny app (deployed to shinyapps.io)
```

## Methods

### 1. Feature engineering (`CensusBureau-API.R`)
Queries the ACS API for raw Census tables (education `B15002`, race `B02001`/`B03001`,
income `B19013`, age `B01001`), bins them into interpretable groups (e.g., five education
levels; five age cohorts), and converts raw counts into **per-county percentage features**
so counties of different sizes are comparable.

### 2. Vote-share regression (`Demographic_Regression.R`)
Harmonizes county FIPS codes, computes **two-party Republican vote share**, then fits a
**separate OLS model for each election year** (2008, 2012, 2016, 2020). Predictors are
**standardized (mean 0, SD 1)** so coefficients are directly comparable in magnitude
across variables and across years. Reports adjusted R², RMSE, and N per year, plus a tidy
coefficient table ranked by effect size.

### 3. Vote-shift analysis (`Trump_Vote_Shift.R`)
Computes each county's Trump vote-share change from 2016 to 2020, **z-scores** the
distribution to flag outlier counties (|z| ≥ 1 and ≥ 2), then runs **Welch two-sample
t-tests** on each demographic variable to identify which characteristics distinguish
counties with large positive vs. large negative shifts, ranked by p-value.

### 4. Interactive app (`App_files/`)
An R Shiny application deployed to shinyapps.io for exploring the demographic-vote
relationships interactively. Visit the live app at https://efried.shinyapps.io/app_files/

## Running Locally

**Prerequisites:** R (≥ 4.0) and the following packages:

```r
install.packages(c("tidyverse", "tidycensus", "broom", "janitor", "shiny", "rsconnect"))
```

**Credentials:** Copy `secrets.example.R` to `secrets.R` and fill in your values.
`secrets.R` is gitignored and should never be committed.

```r
# secrets.R
CENSUS_API_KEY <- "your_key_here"    # https://api.census.gov/data/key_signup.html
SHINY_TOKEN    <- "your_token_here"  # shinyapps.io dashboard → Tokens
SHINY_SECRET   <- "your_secret_here"
SHINY_ACCOUNT  <- "your_account_name"
```

The ACS data has already been pulled and cached in `Data-Final/`, so you can skip
`CensusBureau-API.R` and run the analysis without a Census key.

**Run order:**

```r
source("Demographic_Regression.R")  # per-year vote-share models
source("Trump_Vote_Shift.R")        # 2016→2020 shift analysis
shiny::runApp("App_files")          # launch the interactive app locally
```

To re-pull the ACS data or redeploy the Shiny app, fill in `secrets.R` first, then:

```r
source("CensusBureau-API.R")        # re-pulls ACS data from Census API
# To redeploy:
source("secrets.R")
rsconnect::setAccountInfo(name = SHINY_ACCOUNT, token = SHINY_TOKEN, secret = SHINY_SECRET)
rsconnect::deployApp("App_files")
```

## Tech Stack

`R` · `tidyverse` · `tidycensus` · `broom` · `janitor` · `Shiny` · `shinyapps.io`

## Notes & Limitations

- ACS 5-year estimates are rolling averages, so they approximate (rather than exactly
  match) the population in any single election year.
- The regression is descriptive/associational, not causal; counties are also spatially
  correlated, which OLS does not account for.
- Analysis is at the county level — ecological relationships here should not be read as
  individual-level voting behavior.
