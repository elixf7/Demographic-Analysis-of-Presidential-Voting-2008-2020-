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

The analysis covers ~3,100 U.S. counties and combines API-sourced demographic features with
historical election results, then models the relationship using standardized linear
regression and hypothesis testing.

## Data Sources

| Source | Data | Access |
| --- | --- | --- |
| U.S. Census Bureau — American Community Survey (ACS) 5-year estimates | County-level education, race/ethnicity, median income, and age cohorts for 2009, 2012, 2016, 2020 | Pulled via the [`tidycensus`](https://walker-data.com/tidycensus/) API wrapper |
| MIT Election Data & Science Lab | County Presidential Election Returns, 2000–2020 (`countypres_2000-2020.csv`) | Public dataset |

> The 2009 ACS 5-year period is used as the demographic baseline for the 2008 election.

## Repository Structure

```
.
├── CensusBureau-API.R        # Pulls & engineers ACS demographic features by county
├── Demographic_Regression.R  # Per-year regression of Republican vote share on demographics
├── Trump_Vote_Shift.R        # 2016→2020 vote-shift detection + demographic t-tests
├── Data-Final/               # Cached ACS + election CSVs
└── App_files/                # Interactive R Shiny app (deployed to shinyapps.io)
```

## Methods

### 1. Feature engineering (`CensusBureau-API.R`)
- Queries the ACS API for raw Census tables (education `B15002`, race `B02001`/`B03001`,
  income `B19013`, age `B01001`).
- Bins detailed categories into interpretable groups (e.g., education collapsed into
  *no HS / HS grad / some college / bachelor's / advanced degree*; age into five cohorts).
- Converts counts into per-county **percentage features** so counties of different sizes
  are comparable.

### 2. Vote-share regression (`Demographic_Regression.R`)
- Harmonizes county FIPS codes and computes **two-party Republican vote share** per county.
- Fits a **separate OLS model for each election year** (2008, 2012, 2016, 2020).
- Predictors are **standardized (mean 0, SD 1)** so coefficients are directly comparable
  in magnitude across variables and across years.
- Drops collinear predictors to stabilize estimates.
- Reports **adjusted R², RMSE, and N** per year, plus a tidy table of standardized
  coefficients ranked by effect size.

### 3. Vote-shift analysis (`Trump_Vote_Shift.R`)
- Computes each county's Trump vote share for 2016 and 2020 and the change between them.
- **z-scores** the change to flag counties with unusually large shifts (|z| ≥ 1 / ≥ 2).
- Splits flagged counties into *positive-shift* vs. *negative-shift* groups and runs
  **Welch two-sample t-tests** on each demographic variable to identify which
  characteristics distinguish the two groups, ranked by p-value.

### 4. Interactive app (`App_files/`)
An R Shiny application (deployed to shinyapps.io) that lets users explore the
demographic-vote relationships interactively.

> _Note: expand this section with the app's specific features (which inputs/filters and
> which charts/maps it exposes) once confirmed._

## Running Locally

**Prerequisites:** R (≥ 4.0) and the following packages:

```r
install.packages(c("tidyverse", "tidycensus", "broom", "janitor", "shiny", "rsconnect"))
```

**Census API key** — request a free key at
https://api.census.gov/data/key_signup.html, then set it as an environment variable
(do **not** hardcode it in the scripts):

```r
# In your ~/.Renviron file:
CENSUS_API_KEY=your_key_here
```

```r
# In CensusBureau-API.R:
census_api_key(Sys.getenv("CENSUS_API_KEY"), install = FALSE)
```

**Run order:**

```r
source("CensusBureau-API.R")        # builds the ACS feature table
source("Demographic_Regression.R")  # per-year vote-share models
source("Trump_Vote_Shift.R")        # 2016→2020 shift analysis
shiny::runApp("App_files")          # launch the interactive app
```

## Tech Stack

`R` · `tidyverse` (dplyr, tidyr, stringr, purrr) · `tidycensus` · `broom` · `janitor` ·
`Shiny` · `shinyapps.io`

## Notes & Limitations

- ACS 5-year estimates are rolling averages, so they approximate (rather than exactly
  match) the population in any single election year.
- The regression is descriptive/associational, not causal; counties are also spatially
  correlated, which a basic OLS does not account for.
- Analysis is at the county level — ecological relationships here should not be read as
  individual-level voting behavior.
