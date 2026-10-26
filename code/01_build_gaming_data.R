################################################################################
# 01_build_gaming_data.R
#
# Rebuilds data/final/gaming_data.rds (the analysis dataset used in the paper)
# from the raw Pennsylvania Gaming Control Board (PGCB) monthly revenue reports
# in data/raw/pgcb/, then checks the result against the archived file.
#
# Nedved, J.D. and Ferreira Neto, A.B. (2024). Sports betting cannibalization:
# Evidence from Pennsylvania. Economics Letters 245, 112031.
#
# This script was written after publication to document how the archived
# dataset relates to the raw data. It reproduces the archived dataset exactly,
# including two known data-entry gaps (see Section 4 below and the README).
# It does not change or correct the published data.
#
# Output: data/derived/gaming_data_rebuilt.rds
################################################################################

library(tidyverse)
library(readxl)

if (!file.exists("code/01_build_gaming_data.R")) {
  stop("Run this script from the repository root (the folder containing code/ and data/).")
}

raw_dir <- "data/raw/pgcb"

#-------------------------------------------------------------------------------
# 1. Parse the PGCB reports
#
# Each fiscal-year workbook has one "FY ..." sheet with a block per casino:
# a row with the casino name, then rows of metrics by month. We keep
#   - Slots:          "Gross Terminal Revenue"            -> gtr
#   - Table games:    first "Gross Revenue" (all tables)  -> table_rev
#   - Sports wagering: first "Handle" (total handle)      -> handle
#-------------------------------------------------------------------------------

month_pattern <- paste0("^(January|February|March|April|May|June|July|August|",
                        "September|October|November|December) \\d{4}$")

parse_month <- function(x) {
  x <- str_squish(x)
  ok <- !is.na(x) & str_detect(x, month_pattern)
  out <- as.Date(rep(NA, length(x)))
  out[ok] <- lubridate::my(x[ok])
  out
}

parse_pgcb <- function(path, type) {
  sheets <- excel_sheets(path)
  sheet <- sheets[str_detect(sheets, "^FY")][1]
  m <- as.matrix(suppressMessages(
    read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text")))

  n_months <- apply(m, 1, function(r) sum(!is.na(parse_month(r))))
  hdr      <- which(n_months >= 3)[1]
  months   <- parse_month(m[hdr, ])
  mcols    <- which(!is.na(months))

  label  <- str_squish(m[, 1]); label[label == ""] <- NA
  is_hdr <- n_months >= 3
  n_num  <- apply(m[, mcols, drop = FALSE], 1,
                  function(r) sum(!is.na(suppressWarnings(as.numeric(r)))))

  target <- switch(type,
                   slots  = "^Gross Terminal Revenue",
                   tables = "^Gross Revenue",
                   sports = "^Handle")
  sections <- "^(Total Sports Wagering|Retail Sports Wagering|Online Sports Wagering)"

  out <- list(); casino <- NA; found <- FALSE
  for (i in seq_len(nrow(m))) {
    if (is.na(label[i]) || is_hdr[i]) next
    # A label with no numbers in the month columns starts a new casino block
    if (n_num[i] == 0 && !str_detect(label[i], sections) &&
        !str_detect(label[i], "^(MONTHLY|GTR %)")) {
      casino <- label[i]; found <- FALSE; next
    }
    if (!found && !is.na(casino) && str_detect(label[i], target)) {
      out[[length(out) + 1]] <- tibble(
        type = type, raw_name = casino, date = months[mcols],
        value = suppressWarnings(as.numeric(m[i, mcols])))
      found <- TRUE
    }
  }
  bind_rows(out)
}

files <- list.files(raw_dir, pattern = "\\.xlsx$", full.names = TRUE)
pgcb_long <- map_dfr(files, function(f) {
  type <- case_when(str_detect(f, "Slots")         ~ "slots",
                    str_detect(f, "Table_Games")   ~ "tables",
                    str_detect(f, "Sports_Wagering") ~ "sports")
  parse_pgcb(f, type)
})

#-------------------------------------------------------------------------------
# 2. Harmonize casino names
#
# PGCB names change across years (rebrands and ownership changes). Each raw
# name is mapped to one property:
#   Sands Bethlehem                 -> wind creek
#   SugarHouse                      -> the rivers-philadelphia
#   The Rivers                      -> the rivers-pittsburgh
#   Philadelphia Park / Parx Casino -> parx
#   Harrah's Chester                -> harrah's
#   (Hollywood Casino at) Penn National, "Hollywood Casino" in sports reports
#                                   -> penn national
#   (Hollywood Casino at) The Meadows -> meadows
#   Mohegan Sun / Mohegan Pennsylvania -> mohegan
#-------------------------------------------------------------------------------

harmonize_casino <- function(x) {
  n <- str_to_lower(str_squish(x)) %>% str_replace_all("hlloywood", "hollywood")
  case_when(
    str_detect(n, "total")                     ~ "TOTAL",   # statewide totals
    str_detect(n, "harrah")                    ~ "harrah's",
    str_detect(n, "morgantown")                ~ "hollywood casino morgantown",
    str_detect(n, "york")                      ~ "hollywood casino york",
    str_detect(n, "meadows")                   ~ "meadows",
    str_detect(n, "penn national") | n == "hollywood casino" ~ "penn national",
    str_detect(n, "live.*phila")               ~ "live casino philadelphia",
    str_detect(n, "live.*pitts")               ~ "live pittsburgh",
    str_detect(n, "lehigh")                    ~ "mohegan - lehigh valley",
    str_detect(n, "mohegan")                   ~ "mohegan",
    str_detect(n, "mount airy")                ~ "mount airy",
    str_detect(n, "nemacolin")                 ~ "nemacolin",
    str_detect(n, "malvern")                   ~ "parx at malvern",
    str_detect(n, "shippensburg")              ~ "parx shippensburg",
    str_detect(n, "south phila")               ~ "south philadelphia race and sportsbook",
    str_detect(n, "oaks")                      ~ "oaks race and sportsbook",
    str_detect(n, "^parx|philadelphia park")   ~ "parx",
    str_detect(n, "presque")                   ~ "presque isle",
    str_detect(n, "sugarhouse|rivers.*phila")  ~ "the rivers-philadelphia",
    str_detect(n, "rivers")                    ~ "the rivers-pittsburgh",
    str_detect(n, "sands|wind creek")          ~ "wind creek",
    str_detect(n, "valley ?forge")             ~ "valley forge",
    TRUE ~ NA_character_)
}

pgcb <- pgcb_long %>%
  mutate(casino = harmonize_casino(raw_name)) %>%
  filter(casino != "TOTAL",
         date >= as.Date("2015-07-01"), date <= as.Date("2023-06-01"))

stopifnot(!any(is.na(pgcb$casino)))
stopifnot(nrow(count(pgcb, type, casino, date) %>% filter(n > 1)) == 0)

# One row per casino-month that appears in any report; months a casino does not
# report a given product are set to 0.
nominal <- pgcb %>%
  select(type, casino, date, value) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  transmute(casino, date,
            table_rev = coalesce(tables, 0),
            gtr       = coalesce(slots, 0),
            handle    = coalesce(sports, 0))

#-------------------------------------------------------------------------------
# 3. Deflate and add auxiliary variables
#
# data/raw/deflator.csv: monthly price-level adjustment factor (1 = July 2015),
#   real = nominal * cpi_adjustment. Taken from the archived dataset because the
#   original series file was not retained; see README.
# data/raw/sports_games_count.csv: monthly count of major-league games
#   (count_total). Not used in the published analysis; taken from the archived
#   dataset because the source file was not retained.
# treatment: months relative to November 2018 (first legal sportsbook in PA).
#   Not used in the published analysis, which builds its own event time.
#-------------------------------------------------------------------------------

deflator <- read_csv("data/raw/deflator.csv", show_col_types = FALSE)
games    <- read_csv("data/raw/sports_games_count.csv", show_col_types = FALSE)

gaming_data <- nominal %>%
  mutate(year = lubridate::year(date), month = lubridate::month(date)) %>%
  left_join(games, by = c("year", "month")) %>%
  left_join(deflator, by = c("year", "month")) %>%
  mutate(treatment      = (year - 2018) * 12 + month - 11,
         real_table_rev = table_rev * cpi_adjustment,
         real_gtr       = gtr * cpi_adjustment,
         real_handle    = handle * cpi_adjustment)

#-------------------------------------------------------------------------------
# 4. Reproduce two gaps in the archived dataset
#
# The archived dataset has zeros in two places where the PGCB reports show
# values. The cause is not documented. Both are reproduced here so the rebuilt
# file matches the data behind the published results:
#   (a) meadows, real_table_rev, July 2017 - June 2018 (FY 2017-18)
#   (b) the rivers-philadelphia, real_handle, July 2019 - June 2020 (FY 2019-20)
# To see the values from the PGCB reports instead, set reproduce_archived <- FALSE.
#-------------------------------------------------------------------------------

reproduce_archived <- TRUE

if (reproduce_archived) {
  gaming_data <- gaming_data %>%
    mutate(
      real_table_rev = if_else(casino == "meadows" &
                                 between(date, as.Date("2017-07-01"), as.Date("2018-06-01")),
                               0, real_table_rev),
      real_handle    = if_else(casino == "the rivers-philadelphia" &
                                 between(date, as.Date("2019-07-01"), as.Date("2020-06-01")),
                               0, real_handle))
}

gaming_data <- gaming_data %>%
  arrange(casino, year, month) %>%
  select(casino, month, year, count_total, treatment, cpi_adjustment,
         real_table_rev, real_gtr, real_handle)

dir.create("data/derived", showWarnings = FALSE)
saveRDS(gaming_data, "data/derived/gaming_data_rebuilt.rds")

#-------------------------------------------------------------------------------
# 5. Compare with the archived dataset
#-------------------------------------------------------------------------------

archived <- readRDS("data/final/gaming_data.rds")
key <- c("casino", "year", "month")
a <- archived    %>% arrange(across(all_of(key))) %>% as.data.frame()
b <- gaming_data %>% arrange(across(all_of(key))) %>% as.data.frame()

cat("\nRows: archived =", nrow(a), "| rebuilt =", nrow(b), "\n")
cat("Same casino-month keys:", identical(a[key], b[key]), "\n")
for (v in c("count_total", "treatment", "cpi_adjustment",
            "real_table_rev", "real_gtr", "real_handle")) {
  cat(sprintf("%-15s max abs difference: %.6g\n", v, max(abs(a[[v]] - b[[v]]))))
}
