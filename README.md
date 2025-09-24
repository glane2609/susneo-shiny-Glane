# SUSNEO — Energy & Emissions Shiny App

![CI Status](https://github.com/glane2609/susneo-shiny-Glane/actions/workflows/ci.yml/badge.svg)

A modular Shiny dashboard built with a golem-style structure, using an R6 class for business logic, unit tests, and CI. The app visualizes energy consumption and carbon emissions across multiple facilities.

---

## 1. Setup Instructions

### Clone repository
```bash
git clone https://github.com/glane2609/susneo-shiny-Glane.git
cd susneo-shiny-Glane
```

### Install R packages (run in R / RStudio)
```r
install.packages(c(
  "shiny", "dplyr", "ggplot2", "DT",
  "lubridate", "R6", "testthat", "pkgload"
))
```

### Run the app
Open `app.R` in RStudio and click **Run App**

Or from R console:
```r
shiny::runApp("app.R")
```

---

## 2. App Overview

**Features:**
- Upload CSV or use provided sample data  
- **Filters:** date range selector and facility multi-select  
- **KPIs:** Total consumption, Avg per day/site, Total emissions (kgCO₂e), Peak daily consumption, Facilities selected  
- **Visualizations:** Time series, Comparison (bar), Emissions over time, Energy type share (pie)  
- Summary table for site/type aggregates  

---

## 3. Architecture

Project structure:
```pgsql
susneo-shiny-Glane/
├── R/                           # Modules, BusinessLogic, helpers
│   ├── mod_data_upload.R
│   ├── mod_dashboard.R
│   ├── BusinessLogic.R
│   └── mod_helper.R
├── inst/
│ └── app/
│ └── www/ # Assets (if any)
├── data/
│   └── SAMPLE ASSIGNMENT DATA.csv
├── tests/                       # All test files (including test_all.R)
│   ├── test_all.R               # test wrapper that runs test_dir("tests")
│   ├── test_validation.R
│   ├── test_businesslogic.R
│   └── test_helpers.R
├── .github/workflows/ci.yml
├── app.R
├── DESCRIPTION
└── README.md
```

### Business logic (R6)
`R/BusinessLogic.R` contains an R6 class that encapsulates:
- data filtering  
- KPI calculations (total, avg, peak, emissions)  
- summary data functions  

Keeping calculations in an R6 class keeps server modules thin and easier to test.

---

## 4. Data

Place a sample CSV at `data/SAMPLE ASSIGNMENT DATA.csv`.

Expected columns (helper normalizes names to lowercase / dots):
- `id` (int)  
- `site` (string)  
- `date` (string or Date; helper supports DD-MM-YYYY via lubridate::dmy())  
- `type` (string)  
- `value` (numeric)  
- `carbon emission in kgco2e` (numeric; normalized internally to `carbon.emission.in.kgco2e`)  

---

## 5. Testing

All test-related files (including the test runner `test_all.R`) live in `tests/`.

### Run tests locally (two options)

**A — Using the project test wrapper**
```r
# from project root
source("tests/test_all.R")
```

**B — Using testthat directly**
```r
library(testthat)
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
test_dir("tests")
```

**Included tests:**
- `test_validation.R` — checks data validation and parsing  
- `test_businesslogic.R` — verifies KPI calculations on small sample data  
- `test_helpers.R` — tests helper functions like `fmt_si()`  
- `test_all.R` — optional wrapper that loads package-like code and runs `test_dir("tests")`  

---















