# Replication package: Sports betting cannibalization: Evidence from Pennsylvania

Nedved, Joseph D., and Amir B. Ferreira Neto. 2024. "Sports betting cannibalization: Evidence from Pennsylvania." *Economics Letters* 245: 112031. https://doi.org/10.1016/j.econlet.2024.112031

This repository contains the data and R code behind the published results. The code is posted as it was used for the paper. Known issues are documented below and have not been corrected, so that the package reproduces the published numbers.

## Contents

```
code/
  01_build_gaming_data.R   Rebuilds the analysis dataset from the raw PGCB reports
  02_analysis.R            Produces Tables 1-3, Figures 1-2, and Appendix A-B figures
data/
  raw/pgcb/                PGCB monthly revenue reports (slots, table games, sports wagering), FY2015-16 to FY2023-24
  raw/deflator.csv         Monthly price adjustment factor (July 2015 = 1)
  raw/sports_games_count.csv  Monthly count of major-league games (not used in the analysis)
  raw/online.csv           Casino online sports betting launch dates and county FIPS codes (Appendix A)
  raw/covid.csv            County-level daily Covid-19 cases (Appendix B)
  final/gaming_data.rds    Analysis dataset used for the published results
  derived/                 Output of 01_build_gaming_data.R
output/                    Figures written by 02_analysis.R
```

## How to run

Open R with the repository root as the working directory and run:

```r
source("code/01_build_gaming_data.R")   # optional: rebuilds the dataset and compares it with data/final/
source("code/02_analysis.R")            # results
```

`02_analysis.R` reads `data/final/gaming_data.rds` by default. To run it on the rebuilt dataset instead, change `data_file` at the top of the script. Both give identical estimates.

Tables print to the console: Table 1 (`datasummary`), Table 2 (`t.test` calls), and Table 3 (`ols_*_agg`, `sa_*_agg`, `csa_*_reg` objects). Figures are saved to `output/`. In the published paper, Figure 1 is `gtr-sine-rr.png` and Figure 2 is `table_revenue-sine-rr.png`.

## Data

**Gaming revenues.** Monthly casino-level reports from the Pennsylvania Gaming Control Board (PGCB): slot machine gross terminal revenue, table games gross revenue, and total sports wagering handle. `01_build_gaming_data.R` parses the reports, harmonizes casino names across rebrands (e.g., Sands Bethlehem to Wind Creek, SugarHouse to Rivers Philadelphia, The Rivers to Rivers Pittsburgh), and deflates the series. Revenues in the dataset are in July 2015 dollars. `02_analysis.R` rebases them to June 2023 dollars.

**Deflator and sports-games count.** The original source files for these two monthly series were not retained. Both were extracted from the archived dataset so that the build script runs end to end. The deflator follows a monthly CPI pattern; the paper describes the adjustment as using the GDP deflator.

**Verification.** `01_build_gaming_data.R` reproduces `data/final/gaming_data.rds` exactly (1,444 casino-months, same keys, differences only at floating-point precision).

## Reproducibility notes

Results were reproduced with R 4.3.3 and fixest 0.12.1, did 2.1.2, DRDID 1.0.7, modelsummary 2.1.1, and the tidyverse 2.0.0. OLS (dynamic TWFE) and Sun and Abraham estimates in Table 3, and Tables 1 and 2, reproduce to every reported digit. Callaway and Sant'Anna point estimates reproduce exactly. Their standard errors come from a multiplier bootstrap with no seed set, so they vary slightly from run to run (e.g., 0.016 and 0.027 in the paper versus 0.018 and 0.025 in one rerun).

## Known issues (not corrected)

1. **Number of casinos.** The text describes 12 casinos and the removal of casinos that opened after sports betting was legalized. The code drops five properties by name (three satellite or mini-casinos and two sportsbook-only locations) but keeps the four casinos that opened in 2020-2021 (Live! Philadelphia, Live! Pittsburgh, Hollywood Casino Morgantown, Hollywood Casino York). The estimation sample therefore covers 16 casinos, as stated in the Table 1 note, and these four contribute 116 of the 1,259 observations.
2. **Gaps in the archived dataset.** Two series are zero where the PGCB reports show values:
   - *Table games revenue at The Meadows, July 2017 to June 2018.* The PGCB reports show about $3.3 million per month (nominal) for this period. Filling in these 12 values and rerunning the analysis leaves the Sun and Abraham (0.135) and Callaway and Sant'Anna (0.166) estimates for table games revenue in Table 3 unchanged, because the gap ends six months before The Meadows introduced sports betting (December 2018) and these estimators compare post-treatment months against the month just before treatment. The dynamic TWFE estimate moves from 0.258 (0.075) to 0.274 (0.066). In Table 2, the table revenue differences change from −31.2 to −32.1 (Panel A) and from −8.4 to −8.8 (Panel B). One pre-period estimate in Figure 2 (relative month −6) also shifts slightly. Slot revenue results and the sample size are unaffected.
   - *Sports handle at Rivers Philadelphia, July 2019 to June 2020.* This gap does not change the casino's treatment date, and handle is not an outcome in the published results.

   The build script reproduces both gaps so that the rebuilt dataset matches the published results. Setting `reproduce_archived <- FALSE` in `01_build_gaming_data.R` uses the values from the PGCB reports instead.
3. **Covid shutdown months.** The sample restriction intends to keep April to June 2020 even when all revenues are zero, but the date string it compares against (`"2020-04"`) does not match the format it builds (`"2020-4"`), so the exception never applies. This affects 9 casino-months.
4. **Table 2, Panel B.** `post` is defined as `year >= 2018 & month >= 5`, which classifies January to April of 2019-2023 as pre-period. The published Panel B numbers use this definition.
5. **Unused variable.** `real_table_rev2` is constructed from `real_gtr`. The variable is not used anywhere.
6. **Appendix B sample.** The Covid-control regressions are estimated on the subsample with online launch dates (`df_ob`), which excludes Nemacolin and Presque Isle.

## Citation

If you use this package, please cite the paper above.
