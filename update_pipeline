# =============================================================================
# update_pipeline.R
# NWRFC Monthly Natural Flow Forecast Pre-Fetch Pipeline
#
# Runs daily via GitHub Actions (or manually/locally to validate).
# Fetches all 12 months for all stations in NatFlowWRIAJoin.csv,
# builds a clean wide-format CSV, and writes:
#   data/forecast_latest.csv
#   data/last_updated.txt
#
# Safe-failure design: if too many stations fail, the previous day's
# file is preserved and the script exits with a non-zero status
# (which fails the GHA job and triggers an email notification).
# =============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# -----------------------------------------------------------------------------
# Config
# -----------------------------------------------------------------------------

STATION_LIST_URL <- "https://raw.githubusercontent.com/jeffmarti/data/refs/heads/master/NatFlowWRIAJoin.csv"
OUTPUT_CSV       <- "data/forecast_latest.csv"
OUTPUT_TS        <- "data/last_updated.txt"

# Water year month order
WY_MONTH_LEVELS <- c("OCT", "NOV", "DEC", "JAN", "FEB", "MAR",
                     "APR", "MAY", "JUN", "JUL", "AUG", "SEP")

# Months considered "water supply season" for drought flagging
RUNOFF_MONTHS   <- c("APR", "MAY", "JUN", "JUL", "AUG", "SEP")

# Minimum success rate before we refuse to overwrite previous file
MIN_SUCCESS_RATE <- 0.70   # at least 70% of stations must return usable data

# NOAA base URL pattern
NOAA_URL <- function(id) {
  paste0(
    "https://www.nwrfc.noaa.gov/natural/plot/monthly/",
    "monthly_natural_forecasts.php?id=", id,
    "&datepick=&csv=ESP10&nextwy=0"
  )
}

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

# Determine current water year
current_wy <- function() {
  m <- month(Sys.Date())
  y <- year(Sys.Date())
  if (m >= 10) y + 1L else y
}

# Which months are in the past for the current water year?
# (i.e., observed actuals rather than forecasts)
observed_months <- function() {
  today <- Sys.Date()
  m     <- month(today)
  wy    <- current_wy()
  
  # Build a lookup: month abbrev -> calendar month number
  mon_num <- setNames(1:12, toupper(month.abb))
  
  # A WY month is "past" if its calendar date is before today
  # OCT-DEC belong to WY year - 1; JAN-SEP belong to WY year
  past <- character(0)
  for (mon in WY_MONTH_LEVELS) {
    cal_mon <- mon_num[mon]
    cal_yr  <- if (cal_mon >= 10) wy - 1L else wy
    mon_start <- as.Date(paste(cal_yr, cal_mon, "01", sep = "-"))
    if (mon_start < today) past <- c(past, mon)
  }
  past
}

# Clean raw NOAA dataframe: rename columns, parse numerics, compute monpctnormal
clean_noaa_monthly <- function(df_raw, station_id) {
  df <- df_raw
  
  # Drop stray HTML/header echo lines
  if ("Name" %in% names(df)) {
    df <- df[!grepl("^Name$",   df$Name, perl = TRUE), , drop = FALSE]
    df <- df[!grepl("</pre>",   df$Name, fixed = TRUE), , drop = FALSE]
    df <- df[!is.na(df$Name),            , drop = FALSE]
  }
  
  if (nrow(df) == 0) return(NULL)
  
  # Standardise column names
  ren <- c(
    "ID"              = "NatFlowStationID",
    "90% FCST"        = "e90pct_FCST",
    "75% FCST"        = "e75pct_FCST",
    "50% FCST"        = "e50pct_FCST",
    "25% FCST"        = "e25pct_FCST",
    "10% FCST"        = "e10pct_FCST",
    "30 year average" = "avg_30_year"
  )
  for (nm in names(ren)) {
    if (nm %in% names(df)) names(df)[names(df) == nm] <- ren[[nm]]
  }
  
  # Ensure station ID column exists
  if (!"NatFlowStationID" %in% names(df)) {
    df$NatFlowStationID <- station_id
  }
  
  # Force numeric parsing
  num_cols <- intersect(c("runoff", "e50pct_FCST", "avg_30_year"), names(df))
  for (cc in num_cols) {
    df[[cc]] <- suppressWarnings(readr::parse_number(as.character(df[[cc]])))
  }
  
  # Compute monpctnormal
  if (all(c("avg_30_year", "runoff") %in% names(df))) {
    df$monpctnormal <- if ("e50pct_FCST" %in% names(df)) {
      ifelse(
        !is.na(df$e50pct_FCST) & df$e50pct_FCST > 0,
        df$e50pct_FCST / df$avg_30_year,
        df$runoff      / df$avg_30_year
      )
    } else {
      df$runoff / df$avg_30_year
    }
    df$monpctnormal <- as.numeric(df$monpctnormal)
  } else {
    df$monpctnormal <- NA_real_
  }
  
  # Keep only the columns we need downstream
  keep <- intersect(
    c("NatFlowStationID", "Name", "Month", "monpctnormal"),
    names(df)
  )
  df[keep]
}

# -----------------------------------------------------------------------------
# Step 1: Load station list
# -----------------------------------------------------------------------------

cat("Loading station list...\n")

station_list <- tryCatch(
  readr::read_csv(STATION_LIST_URL, show_col_types = FALSE),
  error = function(e) {
    stop("FATAL: Could not load station list from GitHub.\n  ", conditionMessage(e))
  }
)

# Deduplicate and normalise WRIA names
station_list <- station_list %>%
  filter(!is.na(NatflowID), NatflowID != "") %>%
  mutate(WRIA_NM = str_to_title(trimws(WRIA_NM)))

stations <- unique(station_list$NatflowID)
cat(sprintf("  %d unique stations across %d WRIAs\n",
            length(stations),
            n_distinct(station_list$WRIA_NM)))

# -----------------------------------------------------------------------------
# Step 2: Fetch all stations
# -----------------------------------------------------------------------------

cat("\nFetching NOAA data...\n")

results     <- list()
failed      <- character(0)

for (id in stations) {
  url <- NOAA_URL(id)
  
  df <- tryCatch({
    raw <- suppressWarnings(
      readr::read_csv(url, skip = 2, show_col_types = FALSE)
    )
    clean_noaa_monthly(raw, station_id = id)
  }, error = function(e) {
    NULL
  })
  
  if (is.null(df) || nrow(df) == 0) {
    failed <- c(failed, id)
    cat(sprintf("  [WARN] No data for station: %s\n", id))
  } else {
    results[[id]] <- df
    cat(sprintf("  [OK]   %s  (%d months)\n", id, nrow(df)))
  }
}

# -----------------------------------------------------------------------------
# Step 3: Safety check before overwriting previous file
# -----------------------------------------------------------------------------

n_total   <- length(stations)
n_success <- length(results)
n_failed  <- length(failed)
pct_ok    <- n_success / n_total

cat(sprintf(
  "\nFetch complete: %d/%d stations OK (%.0f%%), %d failed\n",
  n_success, n_total, pct_ok * 100, n_failed
))

if (pct_ok < MIN_SUCCESS_RATE) {
  cat(sprintf(
    "ERROR: Success rate %.0f%% is below threshold %.0f%%.\n",
    pct_ok * 100, MIN_SUCCESS_RATE * 100
  ))
  cat("Previous output files preserved. Exiting with status 1.\n")
  quit(status = 1)
}

if (n_failed > 0) {
  cat(sprintf("NOTE: %d station(s) failed and will appear as NA:\n", n_failed))
  cat(paste0("  ", failed, collapse = "\n"), "\n")
}

# -----------------------------------------------------------------------------
# Step 4: Build long-format dataframe
# -----------------------------------------------------------------------------

cat("\nBuilding combined dataset...\n")

all_long <- bind_rows(results)

# Attach WRIA metadata
wria_lookup <- station_list %>%
  select(NatflowID, WRIA_NR, WRIA_NM) %>%
  distinct()

all_long <- all_long %>%
  left_join(wria_lookup, by = c("NatFlowStationID" = "NatflowID")) %>%
  filter(!is.na(Month))

# Standardise month abbreviations to upper case
all_long <- all_long %>%
  mutate(
    Month = toupper(trimws(Month)),
    # Keep only recognised WY months (drops any garbage rows)
    Month = if_else(Month %in% WY_MONTH_LEVELS, Month, NA_character_)
  ) %>%
  filter(!is.na(Month))

# Tag each row as observed (actual) or forecast
obs_months <- observed_months()
all_long <- all_long %>%
  mutate(
    is_observed    = Month %in% obs_months,
    is_runoff_seas = Month %in% RUNOFF_MONTHS
  )

cat(sprintf("  Long format: %d rows\n", nrow(all_long)))

# -----------------------------------------------------------------------------
# Step 5: Pivot to wide format (one row per station)
# -----------------------------------------------------------------------------

cat("Pivoting to wide format...\n")

# Round before pivoting
all_long <- all_long %>%
  mutate(monpctnormal = round(monpctnormal, 3))

wide <- all_long %>%
  select(WRIA_NR, WRIA_NM, NatFlowStationID, Name, Month, monpctnormal) %>%
  pivot_wider(
    names_from  = Month,
    values_from = monpctnormal
  )

# Enforce water year column order (only include columns that actually exist)
id_cols  <- c("WRIA_NR", "WRIA_NM", "NatFlowStationID", "Name")
mon_cols <- intersect(WY_MONTH_LEVELS, names(wide))
wide     <- wide %>%
  select(all_of(id_cols), all_of(mon_cols)) %>%
  arrange(WRIA_NR, Name)

cat(sprintf("  Wide format: %d rows x %d columns\n", nrow(wide), ncol(wide)))

# -----------------------------------------------------------------------------
# Step 6: Build metadata sidecar
# -----------------------------------------------------------------------------

# Which months are present and their observed/forecast status
month_meta <- tibble(Month = mon_cols) %>%
  mutate(
    is_observed    = Month %in% obs_months,
    is_runoff_seas = Month %in% RUNOFF_MONTHS,
    water_year     = current_wy()
  )

# -----------------------------------------------------------------------------
# Step 7: Write outputs
# -----------------------------------------------------------------------------

cat("\nWriting outputs...\n")

# Create data/ directory if it doesn't exist (local runs)
if (!dir.exists("data")) dir.create("data")

# Main forecast table
write_csv(wide, OUTPUT_CSV, na = "")
cat(sprintf("  Wrote: %s\n", OUTPUT_CSV))

# Month metadata (used by app to shade observed vs forecast columns)
write_csv(month_meta, "data/month_meta.csv")
cat("  Wrote: data/month_meta.csv\n")

# Timestamp
ts <- format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC")
writeLines(ts, OUTPUT_TS)
cat(sprintf("  Wrote: %s  (%s)\n", OUTPUT_TS, ts))

# Pipeline summary (useful for GHA logs)
summary_lines <- c(
  paste0("Pipeline run:      ", ts),
  paste0("Water year:        ", current_wy()),
  paste0("Stations fetched:  ", n_success, "/", n_total),
  paste0("Months present:    ", paste(mon_cols, collapse = ", ")),
  paste0("Observed months:   ", paste(obs_months[obs_months %in% mon_cols], collapse = ", ")),
  paste0("Forecast months:   ", paste(setdiff(mon_cols, obs_months), collapse = ", "))
)
if (n_failed > 0) {
  summary_lines <- c(summary_lines,
    paste0("Failed stations:   ", paste(failed, collapse = ", ")))
}
writeLines(summary_lines, "data/pipeline_summary.txt")
cat("\nPipeline summary:\n")
cat(paste0("  ", summary_lines, "\n"), sep = "")

cat("\nDone.\n")
