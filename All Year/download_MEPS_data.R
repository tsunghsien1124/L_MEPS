# Load necessary libraries
rm(list = ls())
library(MEPS)
library(haven)

# File directory ---------------------------------------------------------------
username <- "user" # "Tsung-Hsien Li"
setwd(paste0("C:/Users/", username, "/Documents/GitHub/L_MEPS/All Year/"))
dic_data_raw <- paste0(getwd(), "/Raw Data/")
dic_data_adj <- paste0(getwd(), "/Raw Data (Adjusted)/")

# Create data of aggregate moments from NHED and AHA ---------------------------
aggregate_data <- data.frame(
  Year = 1996:2020,
  NHED_OOPMEDEX = c(
    146076 * 10 ^ 6,
    156144 * 10 ^ 6,
    170998 * 10 ^ 6,
    180883 * 10 ^ 6,
    193555 * 10 ^ 6,
    200879 * 10 ^ 6,
    219144 * 10 ^ 6,
    235149 * 10 ^ 6,
    248415 * 10 ^ 6,
    264486 * 10 ^ 6,
    277950 * 10 ^ 6,
    293596 * 10 ^ 6,
    300063 * 10 ^ 6,
    296656 * 10 ^ 6,
    301469 * 10 ^ 6,
    310195 * 10 ^ 6,
    323198 * 10 ^ 6,
    330965 * 10 ^ 6,
    340811 * 10 ^ 6,
    353504 * 10 ^ 6,
    365936 * 10 ^ 6,
    372891 * 10 ^ 6,
    386811 * 10 ^ 6,
    402950 * 10 ^ 6,
    392291 * 10 ^ 6
  ),
  AHA_UNHCC = c(
    18.00 * 10 ^ 9,
    18.50 * 10 ^ 9,
    19.00 * 10 ^ 9,
    20.70 * 10 ^ 9,
    21.60 * 10 ^ 9,
    21.50 * 10 ^ 9,
    22.40 * 10 ^ 9,
    24.90 * 10 ^ 9,
    27.00 * 10 ^ 9,
    29.30 * 10 ^ 9,
    31.60 * 10 ^ 9,
    34.40 * 10 ^ 9,
    36.80 * 10 ^ 9,
    39.50 * 10 ^ 9,
    39.80 * 10 ^ 9,
    41.60 * 10 ^ 9,
    46.30 * 10 ^ 9,
    46.80 * 10 ^ 9,
    43.20 * 10 ^ 9,
    36.10 * 10 ^ 9,
    38.40 * 10 ^ 9,
    38.40 * 10 ^ 9,
    41.30 * 10 ^ 9,
    41.61 * 10 ^ 9,
    42.67 * 10 ^ 9
  ),
  NHED_TOTHS = c(
    350813 * 10 ^ 6,
    363403 * 10 ^ 6,
    374911 * 10 ^ 6,
    393630 * 10 ^ 6,
    415532 * 10 ^ 6,
    449360 * 10 ^ 6,
    486482 * 10 ^ 6,
    525892 * 10 ^ 6,
    565327 * 10 ^ 6,
    608600 * 10 ^ 6,
    651209 * 10 ^ 6,
    691887 * 10 ^ 6,
    721630 * 10 ^ 6,
    771040 * 10 ^ 6,
    808795 * 10 ^ 6,
    833246 * 10 ^ 6,
    877968 * 10 ^ 6,
    906804 * 10 ^ 6,
    940526 * 10 ^ 6,
    988971 * 10 ^ 6,
    1035398 * 10 ^ 6,
    1077580 * 10 ^ 6,
    1122658 * 10 ^ 6,
    1193588 * 10 ^ 6,
    1267833 * 10 ^ 6
  ),
  NHED_TOTMEDEX = c(
    914642 * 10 ^ 6,
    965620 * 10 ^ 6,
    1019190 * 10 ^ 6,
    1078770 * 10 ^ 6,
    1156548 * 10 ^ 6,
    1256534 * 10 ^ 6,
    1365481 * 10 ^ 6,
    1475864 * 10 ^ 6,
    1582380 * 10 ^ 6,
    1693830 * 10 ^ 6,
    1806451 * 10 ^ 6,
    1921490 * 10 ^ 6,
    2007153 * 10 ^ 6,
    2105465 * 10 ^ 6,
    2180461 * 10 ^ 6,
    2253897 * 10 ^ 6,
    2346203 * 10 ^ 6,
    2405174 * 10 ^ 6,
    2527085 * 10 ^ 6,
    2674257 * 10 ^ 6,
    2795557 * 10 ^ 6,
    2903812 * 10 ^ 6,
    3019768 * 10 ^ 6,
    3173081 * 10 ^ 6,
    3366975 * 10 ^ 6
  )
)
saveRDS(dt, file = paste0(dic_data_raw, 'aggregate_data.RData'))

# Function to download and save all MEPS raw data ------------------------------
download_MEPS_data <- function(year, dic) {
  dt <- read_MEPS(year = year, type = 'FYC')
  saveRDS(dt, file = paste0(dic, 'MEPS_', year, '.RData'))
}
options(timeout = 300) # 5 mins
years <- 1996:2020
# lapply(years, function(year) download_MEPS_data(year, dic_data_raw))

# Create new MEPS data with selective variables --------------------------------
create_new_data <- function(years, dic, aggregate_data) {
  # Loop over all years interested
  for (year in years) {
    # Load MEPS raw data
    yr <- substr(year, 3, 4)
    dt <- readRDS(paste0(dic_data_raw, 'MEPS_', year, '.RData'))
    
    # Adjust inconsistent variable name for total income in Year 1996
    if (year == 1996) {
      dt$INCOME <- dt$TTLPNX
    } else {
      dt$INCOME <- dt[[paste0('TTLP', yr, 'X')]]
    }
    
    # Select the appropriate personal weight
    if (year < 1999) {
      dt$WGTNEW <- dt[[paste0('WTDPER', yr)]]
    } else {
      dt$WGTNEW <- dt[[paste0('PERWT', yr, 'F')]]
    }
    
    # Identify respondents not insured
    dt$NOINS <-
      rowSums(sapply(c(
        'JA',
        'FE',
        'MA',
        'AP',
        'MY',
        'JU',
        'JL',
        'AU',
        'SE',
        'OC',
        'NO',
        'DE'
      ), function(mo)
        as.numeric(dt[[paste0('INS', mo, yr, 'X')]] != 1))) > 0
    dt$NOINS_all <- dt[[paste0('INSCOV', yr)]] == 3
    
    # Compute total charges and differences between charges and expenditures
    dt$ALL <- dt[[paste0('TOTTCH', yr)]] + dt[[paste0('RXEXP', yr)]]
    dt$DIFF <- dt$ALL - dt[[paste0('TOTEXP', yr)]]
    dt$DIFF <- dt$NOINS * (dt$DIFF > 0) * dt$DIFF
    
    # MEPS total out-of-pocket expenditures
    MEPS.OOPMEDEX <- sum(dt[[paste0('TOTSLF', yr)]] * dt$WGTNEW, na.rm = TRUE)
    
    # NHED total national health expenditures (out-of-pocket)
    NHED.OOPMEDEX <- aggregate_data$NHED_OOPMEDEX[aggregate_data$Year == year]
    
    # Factor alpha
    factor.OOP <- NHED.OOPMEDEX / MEPS.OOPMEDEX
    
    # AHA uncompensated hospital care cost
    AHA.UNHCC <- aggregate_data$AHA_UNHCC[aggregate_data$Year == year]
    
    # NHED total hospital spending
    NHED.TOTHS <- aggregate_data$NHED_TOTHS[aggregate_data$Year == year]
    
    # Share of bad debt
    BD.SHARE <- AHA.UNHCC / NHED.TOTHS
    
    # NHED total personal health care expenditures
    NHED.TOTMEDEX <- aggregate_data$NHED_TOTMEDEX[aggregate_data$Year == year]
    
    # Total bad debt
    TOTBD <- BD.SHARE * NHED.TOTMEDEX
    
    # Factor beta
    factor.BD <- TOTBD / sum(dt$DIFF * dt$WGTNEW, na.rm = TRUE)
    
    # Compute person-level adjusted out-of-pocket medical expenses
    dt$ADJOOP <-
      factor.OOP * dt[[paste0('TOTSLF', yr)]] +
      factor.BD * dt$NOINS * dt$DIFF
    
    # Family ID
    dt$DUIDFAMY <- paste(dt$DUID, dt$FAMIDYR, sep = "")
    
    # Select the appropriate family weight
    if (year < 1999) {
      dt$FAMYWGT <- dt[[paste0('WTFAMF', yr)]]
    } else {
      dt$FAMYWGT <- dt[[paste0('FAMWT', yr, 'F')]]
    }

    # Create new data 
    dt_new <- data.frame(
      YEAR = rep(year, dim(dt)[1]),
      WGT = dt$WGTNEW,
      AGE = dt[[paste0('AGE', yr, 'X')]],
      SEX = dt$SEX,
      INCOME = dt$INCOME,
      NOINS = dt$NOINS,
      NOINS_all = dt$NOINS_all,
      DIFF = dt$DIFF,
      OOP = dt[[paste0('TOTSLF', yr)]],
      ADJ_OOP = dt$ADJOOP,
      FAMYID = dt$DUIDFAMY,
      FAMYREF = dt$FAMRFPYR,
      FAMYWGT = dt$FAMYWGT
    )
    
    # Save the new data
    saveRDS(dt_new, file = paste0(dic, 'MEPS_', year, '_new.RData'))
  }
}
create_new_data(years, dic_data_adj, aggregate_data)

# Create new aggregate date with all new MEPS data -----------------------------
file.list <- lapply(years, function(year)
  paste0(dic_data_adj, 'MEPS_', year, '_new.RData'))
MEPS_all <- do.call(rbind, lapply(file.list, readRDS))
saveRDS(MEPS_all, file = paste0(dic_data_adj, 'MEPS_all.RData'))

# OLD CODE (VERY SLOW!) --------------------------------------------------------
# # Merge all MEPS data with selective variables
# merge_all_data <- function(years, dic) {
#
#   # First survey year
#   year <- years[1]
#   yr <- substr(year, 3, 4)
#   dt <- readRDS(paste0(dic, 'MEPS_', year, '.RData'))
#   MEPS_all <- data.frame(
#     Year = rep(year, dim(dt)[1]),
#     WGT = dt$WGTNEW,
#     NOINS = dt$NOINS,
#     DIFF = dt$DIFF,
#     OOP = dt[[paste0('TOTSLF', yr)]],
#     ADJ_OOP = dt$ADJOOP
#   )
#
#   # Onward survey years
#   for (year_i in 2:length(years)) {
#     year <- years[year_i]
#     yr <- substr(year, 3, 4)
#     dt <- readRDS(paste0(dic, 'MEPS_', year, '.RData'))
#     dt_new <- data.frame(
#       Year = rep(year, dim(dt)[1]),
#       WGT = dt$WGTNEW,
#       NOINS = dt$NOINS,
#       DIFF = dt$DIFF,
#       OOP = dt[[paste0('TOTSLF', yr)]],
#       ADJ_OOP = dt$ADJOOP
#     )
#     MEPS_all <- rbind(MEPS_all, dt_new)
#   }
#
#   # Save the new aggregate data
#   saveRDS(MEPS_all, file = paste0(dic, 'MEPS_all.RData'))
# }
# merge_all_data(years, dic)

# Function to download and save all MEPS data from 1996 to 2020-----------------
# download_all_data <- function(year, dic, aggregate_data) {
#
#   # Download MEPS data
#   dt <- read_MEPS(year = year, type = 'FYC')
#   yr <- substr(year, 3, 4)
#
#   # Adjust inconsistent variable name for total income in Year 1996
#   if (year == 1996) {
#     dt$INCOME <- dt$TTLPNX
#   } else {
#     dt$INCOME <- dt[[paste0('TTLP', yr, 'X')]]
#   }
#
#   # Select the appropriate personal weight
#   if (year < 1999) {
#     dt$WGTNEW <- dt[[paste0('WTDPER', yr)]]
#   } else {
#     dt$WGTNEW <- dt[[paste0('PERWT', yr, 'F')]]
#   }
#
#   # Identify respondents not insured
#   dt$NOINS <-
#     rowSums(
#       sapply(c('JA', 'FE', 'MA', 'AP', 'MY', 'JU', 'JL', 'AU', 'SE', 'OC', 'NO', 'DE'),
#              function(mo) as.numeric(dt[[paste0('INS', mo, yr, 'X')]] != 1))
#     ) > 0
#
#   # Compute total charges and differences between charges and expenditures
#   dt$ALL <- dt[[paste0('TOTTCH', yr)]] + dt[[paste0('RXEXP', yr)]]
#   dt$DIFF <- dt$ALL - dt[[paste0('TOTEXP', yr)]]
#   dt$DIFF <- dt$NOINS * (dt$DIFF > 0) * dt$DIFF
#
#   # MEPS total out-of-pocket expenditures
#   MEPS.OOPMEDEX <- sum(dt[[paste0('TOTSLF', yr)]] * dt$WGTNEW, na.rm = TRUE)
#
#   # NHED total national health expenditures (out-of-pocket)
#   NHED.OOPMEDEX <- aggregate_data$NHED_OOPMEDEX[aggregate_data$Year == year]
#
#   # Factor alpha
#   factor.OOP <- NHED.OOPMEDEX / MEPS.OOPMEDEX
#
#   # AHA uncompensated hospital care cost
#   AHA.UNHCC <- aggregate_data$AHA_UNHCC[aggregate_data$Year == year]
#
#   # NHED total hospital spending
#   NHED.TOTHS <- aggregate_data$NHED_TOTHS[aggregate_data$Year == year]
#
#   # Share of bad debt
#   BD.SHARE <- AHA.UNHCC / NHED.TOTHS
#
#   # NHED total personal health care expenditures
#   NHED.TOTMEDEX <- aggregate_data$NHED_TOTMEDEX[aggregate_data$Year == year]
#
#   # Total bad debt
#   TOTBD <- BD.SHARE * NHED.TOTMEDEX
#
#   # Factor beta
#   factor.BD <- TOTBD / sum(dt$DIFF * dt$WGTNEW, na.rm = TRUE)
#
#   # Compute person-level adjusted out-of-pocket medical expenses
#   dt$ADJOOP <-
#     factor.OOP * dt[[paste0('TOTSLF', yr)]] +
#     factor.BD * dt$NOINS * dt$DIFF
#
#   # Save the new data
#   saveRDS(dt, file = paste0(dic, 'MEPS_', year, '.RData'))
# }
#
# # List of years to download and apply the function over each year---------------
# years <- 1996:2020
#
# # Download MEPS data across years-----------------------------------------------
# # lapply(years, function(year) download_all_data(year, dic, aggregate_data))
