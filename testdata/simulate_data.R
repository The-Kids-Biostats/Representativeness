# make_test_data.R -----------------------------------------------------
# Generates a test "population" dataset (n = 300) with mixed variable types
# and some structured missingness, then saves as .csv, .xlsx, .RData.

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(writexl)
  library(readr)
})

set.seed(20260113)

n <- 300

# --- Core IDs / structure ----
dat <- tibble(
  id = sprintf("ID%04d", 1:n),
  
  # A simple cluster variable (e.g., site / school / clinic)
  site = factor(sample(paste0("Site_", LETTERS[1:6]), n, replace = TRUE,
                       prob = c(0.18, 0.16, 0.15, 0.19, 0.17, 0.15))),
  
  # Dates
  enrol_date = as.Date("2023-01-01") + sample(0:900, n, replace = TRUE),
  
  # Demographics / mixed types
  age = pmin(pmax(round(rnorm(n, mean = 42, sd = 13)), 18), 80),
  sex = factor(sample(c("Female", "Male", "Other"), n, replace = TRUE,
                      prob = c(0.51, 0.47, 0.02))),
  region = factor(sample(c("Metro", "Regional", "Remote"), n, replace = TRUE,
                         prob = c(0.72, 0.23, 0.05))),
  
  # Ordinal variable
  education = ordered(
    sample(c("Year 10 or less", "Year 12", "Certificate/Diploma", "Bachelor+", "Postgrad"),
           n, replace = TRUE,
           prob = c(0.18, 0.26, 0.27, 0.20, 0.09)),
    levels = c("Year 10 or less", "Year 12", "Certificate/Diploma", "Bachelor+", "Postgrad")
  ),
  
  # Continuous variables (some skewed)
  bmi = round(rnorm(n, mean = 27.2, sd = 5.1), 1),
  income_aud = round(exp(rnorm(n, mean = log(65000), sd = 0.55))),  # skewed
  score_std = round(rnorm(n, mean = 0, sd = 1), 3),
  
  # Binary/logical
  smoker = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.18, 0.82)),
  chronic_condition = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.32, 0.68)),
  
  # Count variable (overdispersed-ish)
  n_visits_last_year = rpois(n, lambda = 2.4)
)

# --- Add a few more features with dependency structure ----
# Site-level random effect (just to create cluster-correlated outcomes)
site_re <- rnorm(nlevels(dat$site), mean = 0, sd = 0.6)
names(site_re) <- levels(dat$site)

dat <- dat %>%
  mutate(
    # A bounded outcome (0-100) influenced by age, education, site, chronic condition
    wellbeing_score = 50 +
      0.12 * (age - 40) +
      as.numeric(education) * 2.2 +
      site_re[as.character(site)] * 6 -
      if_else(chronic_condition, 6, 0) +
      rnorm(n, 0, 8),
    
    wellbeing_score = pmin(pmax(round(wellbeing_score, 1), 0), 100),
    
    # A categorical variable with more levels
    transport_mode = factor(sample(
      c("Car", "Public transport", "Walk", "Bike", "Other"),
      n, replace = TRUE, prob = c(0.62, 0.18, 0.10, 0.06, 0.04)
    )),
    
    # A character column (free text-ish)
    notes = sample(
      c("prefers morning", "prefers afternoon", "needs interpreter", "no notes", ""),
      n, replace = TRUE, prob = c(0.14, 0.16, 0.06, 0.54, 0.10)
    )
  )

# --- Introduce structured missingness (useful for testing) ----
# e.g., income missing more often in Remote, BMI missing more often in smokers
set.seed(20260113 + 1)
dat <- dat %>%
  mutate(
    income_aud = if_else(region == "Remote" & runif(n) < 0.25, NA_real_, income_aud),
    bmi        = if_else(smoker & runif(n) < 0.18, NA_real_, bmi),
    notes      = if_else(runif(n) < 0.05, NA_character_, notes)
  )

# --- Make sure types are nice for export ----
# (Excel + CSV can be fussy with ordered factors/dates; we keep them)
# You can also add a numeric-only copy if needed.

# --- Output paths ----
out_dir <- "testdata"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# --- Full datasts ----

csv_path  <- file.path(out_dir, "population_testdata_n300.csv")
xlsx_path <- file.path(out_dir, "population_testdata_n300.xlsx")
rdata_path <- file.path(out_dir, "population_testdata_n300.RData")

# --- Save files ----
readr::write_csv(dat, csv_path, na = "")
writexl::write_xlsx(list(population = dat), xlsx_path)

population_testdata <- dat
save(population_testdata, file = rdata_path)

message("Saved:\n- ", csv_path, "\n- ", xlsx_path, "\n- ", rdata_path)

# --- Sample datasets ----

dat_sample <- dat[sample(nrow(dat), 50), ]

csv_path  <- file.path(out_dir, "sample_testdata_n50.csv")
xlsx_path <- file.path(out_dir, "sample_testdata_n50.xlsx")
rdata_path <- file.path(out_dir, "sample_testdata_n50.RData")

# --- Save files ----
readr::write_csv(dat_sample, csv_path, na = "")
writexl::write_xlsx(list(population = dat_sample), xlsx_path)

population_testdata <- dat_sample
save(population_testdata, file = rdata_path)

message("Saved:\n- ", csv_path, "\n- ", xlsx_path, "\n- ", rdata_path)
