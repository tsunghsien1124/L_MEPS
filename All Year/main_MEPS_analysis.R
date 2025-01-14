# Load necessary libraries
rm(list = ls())
library(dplyr)
library(Hmisc)
library(matrixStats)
library(ggplot2)
theme_set(theme_bw())
library(ggpattern)
library(viridis)
library(reldist)

# Data directory----------------------------------------------------------------
username <- "user" # "Tsung-Hsien Li"
setwd(paste0("C:/Users/", username, "/Documents/GitHub/L_MEPS/All Year/"))
dic_data_raw <- paste0(getwd(), "/Raw Data/")
dic_data_adj <- paste0(getwd(), "/Raw Data (Adjusted)/")
dic_fig <- paste0(getwd(), "/Figure/")

# Load MEPS data ---------------------------------------------------------------
MEPS_all <- readRDS(paste0(dic_data_adj, "MEPS_all.RData"))
MEPS_all <- MEPS_all[MEPS_all$YEAR < 2020, ]
MEPS_all <- MEPS_all[MEPS_all$INCOME > 0, ]
MEPS_all <- MEPS_all[MEPS_all$AGE >= 20, ]
MEPS_all <- MEPS_all[MEPS_all$WGT > 0, ]
rownames(MEPS_all) <- 1:dim(MEPS_all)[1]

# Create auxiliary variables ---------------------------------------------------
figure.width <- 6
figure.height <- 4.0
years <- 1996:2019
year.break <- seq(years[1], years[length(years)], by = 4)
year.group <- c("1996-1999",
                "2000-2003",
                "2004-2007",
                "2008-2011",
                "2012-2015",
                "2016-2019")
year.group.name <- c("96-99", "00-03", "04-07", "08-11", "12-15", "16-19")
year.group.num <- length(year.group)

# year.break <- seq(years[1], years[length(years)], by = 3)
# year.group <- c(
#   "1996-1998",
#   "1999-2001",
#   "2002-2004",
#   "2005-2007",
#   "2008-2010",
#   "2011-2013",
#   "2014-2016",
#   "2017-2019"
# )
# year.group.name <- c("96-98",
#                      "99-01",
#                      "02-04",
#                      "05-07",
#                      "08-10",
#                      "11-13",
#                      "14-16",
#                      "17-19")
# year.group.num <- length(year.group)

# year.combined.group <- c("1996-2003", "2004-2011", "2012-2019")
# year.combined.group.name <- c("96-03", "04-11", "12-19")
# year.combined.group <- c("1996-2004", "2005-2013", "2014-2019")
# year.combined.group.name <- c("96-04", "05-13", "14-19")
year.combined.group <- c("1996-2013", "2014-2019")
year.combined.group.name <- c("96-13", "14-19")
year.combined.group.num <- length(year.combined.group)
age.group <- c("20-34", "35-44", "45-54", "55-64", "65-74", "75+")
age.group.num <- length(age.group)
age.combined.group <- c("20-44", "45-64", "65+")
age.combined.group.num <- length(age.combined.group)

MEPS_all$ADJ_OOP_INC <- MEPS_all$ADJ_OOP / MEPS_all$INCOME

inc.group.num <- 10
inc.combined.group.num <- 4

# Create auxiliary functions ---------------------------------------------------
wtd.quantile.group <- function(x, w, n) {
  results <- c()
  for (i in seq(n + 1)) {
    if (i == 1) {
      results <- c(results, -Inf)
    } else if (i == n + 1) {
      results <- c(results, Inf)
    } else {
      results <- c(results, reldist::wtd.quantile(x, q = (i - 1) / n, weight = w))
    }
  }
  return(results)
}
wtd.quantcut <- function(x, w, n) {
  return(cut(
    x,
    breaks = wtd.quantile.group(x, w, n),
    include.lowest = TRUE,
    labels = c(1:n)
  ))
}

# Create groups ------------------------------------------------------------
MEPS_all$YEAR_G <- cut(
  MEPS_all$YEAR,
  breaks = c(1996, 1999, 2003, 2007, 2011, 2015, Inf),
  labels = c('1', '2', '3', '4', '5', '6'),
  # breaks = c(1996, 1998, 2001, 2004, 2007, 2010, 2013, 2016, Inf),
  # labels = c('1', '2', '3', '4', '5', '6', '7', '8'),
  include.lowest = TRUE
)
MEPS_all %>% dplyr::count(YEAR_G)

MEPS_all$YEAR_CG <- cut(
  MEPS_all$YEAR,
  # breaks = c(1996, 2003, 2011, Inf),
  # breaks = c(1996, 2004, 2013, Inf),
  breaks = c(1996, 2013, Inf),
  # labels = c('1', '2', '3'),
  labels = c('1', '2'),
  include.lowest = TRUE
)
MEPS_all %>% dplyr::count(YEAR_CG)

MEPS_all$AGE_G <- cut(
  MEPS_all$AGE,
  breaks = c(20, 34, 44, 54, 64, 74, Inf),
  labels = c('1', '2', '3', '4', '5', '6'),
  include.lowest = TRUE
)
MEPS_all %>% dplyr::count(AGE_G)

MEPS_all$AGE_CG <- cut(
  MEPS_all$AGE,
  breaks = c(20, 44, 64, Inf),
  labels = c('1', '2', '3'),
  include.lowest = TRUE
)
MEPS_all %>% dplyr::count(AGE_CG)

# wtd.quantile.group(MEPS_all$INCOME, MEPS_all$WGT, inc.group.num)
MEPS_all$INCOME_G <- wtd.quantcut(MEPS_all$INCOME, MEPS_all$WGT, inc.group.num)
MEPS_all %>% dplyr::count(INCOME_G)

MEPS_all$INCOME_CG <- wtd.quantcut(MEPS_all$INCOME, MEPS_all$WGT, inc.combined.group.num)
MEPS_all %>% dplyr::count(INCOME_CG)

# Share of Uninsured -----------------------------------------------------------
results.NOINS.year <- c()
results.NOINS.status <- c()
results.NOINS.average <- c()
for (year in years) {
  results.NOINS.year <- c(results.NOINS.year, rep(year, 2))
  results.NOINS.status <- c(results.NOINS.status, c("Any Month", "Entire Year"))
  data.temp <- MEPS_all[MEPS_all$YEAR == year, ]
  results.NOINS.average <- c(results.NOINS.average,
                             wtd.mean(data.temp$NOINS, data.temp$WGT))
  results.NOINS.average <- c(results.NOINS.average,
                             wtd.mean(data.temp$NOINS_all, data.temp$WGT))
}
rm(year, data.temp)
gc()
results.NOINS <- data.frame(
  year = as.character(results.NOINS.year),
  status = factor(results.NOINS.status),
  average = results.NOINS.average * 100
)
rm(results.NOINS.year,
   results.NOINS.status,
   results.NOINS.average)
par(mar = c(0, 0, 0, 0))
ggplot(data = results.NOINS, aes(
  x = year,
  y = average,
  group = status,
  color = status
)) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year", y = "%") +
  scale_x_discrete(breaks = year.break)
dev.copy(pdf,
         paste0(dic_fig, "NOINS_share.pdf"),
         width = figure.width,
         height = figure.height + 0.5)
dev.off()

# Share of Uninsured by Age (C) Group ------------------------------------------
results.NOINS.AGE_G.year <- c()
results.NOINS.AGE_G.age_group <- c()
results.NOINS.AGE_G.average <- c()
for (year in years) {
  results.NOINS.AGE_G.year <- c(results.NOINS.AGE_G.year,
                                rep(year, age.combined.group.num))
  for (age in sequence(age.combined.group.num)) {
    results.NOINS.AGE_G.age_group <- c(results.NOINS.AGE_G.age_group, age.combined.group[age])
    data.temp <- MEPS_all[(MEPS_all$YEAR == year) &
                            (MEPS_all$AGE_CG == age), ]
    results.NOINS.AGE_G.average <- c(results.NOINS.AGE_G.average,
                                     wtd.mean(data.temp$NOINS, data.temp$WGT))
  }
}
rm(year, age, data.temp)
gc()
results.NOINS.AGE_G <- data.frame(
  year = as.character(results.NOINS.AGE_G.year),
  age_group = factor(results.NOINS.AGE_G.age_group),
  average = results.NOINS.AGE_G.average * 100
)
rm(
  results.NOINS.AGE_G.year,
  results.NOINS.AGE_G.age_group,
  results.NOINS.AGE_G.average
)
par(mar = c(0, 0, 0, 0))
ggplot(data = results.NOINS.AGE_G,
       aes(
         x = year,
         y = average,
         group = age_group,
         color = age_group
       )) +
  geom_line(linewidth = 1, aes(linetype = age_group)) +
  geom_point(size = 3, aes(shape = age_group)) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year", y = "%") +
  scale_x_discrete(breaks = year.break)
dev.copy(
  pdf,
  paste0(dic_fig, "NOINS_share_AGE_CG.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()

# Share of Uninsured by Income (C) Group ---------------------------------------
results.NOINS.INC_G.year <- c()
results.NOINS.INC_G.inc_group <- c()
results.NOINS.INC_G.average <- c()
for (year in years) {
  results.NOINS.INC_G.year <- c(results.NOINS.INC_G.year,
                                rep(year, inc.combined.group.num))
  for (inc in sequence(inc.combined.group.num)) {
    results.NOINS.INC_G.inc_group <- c(results.NOINS.INC_G.inc_group, inc)
    data.temp <- MEPS_all[(MEPS_all$YEAR == year) &
                            (MEPS_all$INCOME_CG == inc), ]
    results.NOINS.INC_G.average <- c(results.NOINS.INC_G.average,
                                     wtd.mean(data.temp$NOINS, data.temp$WGT))
  }
}
rm(year, inc, data.temp)
gc()
results.NOINS.INC_G <- data.frame(
  year = as.character(results.NOINS.INC_G.year),
  inc_group = factor(results.NOINS.INC_G.inc_group),
  average = results.NOINS.INC_G.average * 100
)
rm(
  results.NOINS.INC_G.year,
  results.NOINS.INC_G.inc_group,
  results.NOINS.INC_G.average
)
par(mar = c(0, 0, 0, 0))
ggplot(data = results.NOINS.INC_G,
       aes(
         x = year,
         y = average,
         group = inc_group,
         color = inc_group
       )) +
  geom_line(linewidth = 1, aes(linetype = inc_group)) +
  geom_point(size = 3, aes(shape = inc_group)) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year", y = "%") +
  scale_x_discrete(breaks = year.break)
dev.copy(
  pdf,
  paste0(dic_fig, "NOINS_share_INC_CG.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()


# OOP --------------------------------------------------------------------------
# results.OOP_Mean <- c()
# for (year in years) {
#   results.OOP_Mean <- c(results.OOP_Mean, wtd.mean(MEPS_all$OOP[MEPS_all$Year == year], MEPS_all$WGT[MEPS_all$Year == year]))
# }
# results.OOP_Mean <- data.frame(year = factor(years), average = results.OOP_Mean)
# par(mar = c(0, 0, 0, 0))
# ggplot(data = results.OOP_Mean, aes(x = year, y = average, group = 1)) +
#   geom_line(color = "blue", linewidth = 1) +
#   geom_point(color = "blue", size = 3) +
#   theme(
#         legend.position = "top",  legend.key.width = unit(1.3, "cm"),
#     text = element_text(size = 18, family = "serif"),
#     legend.title = element_text(size = 15),
#     legend.text = element_text(size = 16),
#     axis.text.x = element_text(size = 13),
#     axis.text.y = element_text(size = 13)
#   ) +
#   labs(x = "year", y = "$") +
#   scale_x_discrete(breaks = year.break)
# dev.copy(pdf,
#          paste0(dic_fig, "OOP_mean.pdf"),
#          width = figure.width,
#          height = figure.height)
# dev.off()

# OOP by Insurance Status --------------------------------------------
results.OOP_INS.year <- c()
results.OOP_INS.status <- c()
results.OOP_INS.average <- c()
results.OOP_INS.ratio <- c()
for (year in years) {
  results.OOP_INS.year <- c(results.OOP_INS.year, rep(year, 3))
  results.OOP_INS.status <- c(results.OOP_INS.status, c("All", "Insured", "Uninsured"))
  data.temp <- MEPS_all[MEPS_all$YEAR == year, ]
  OOP_INS_avg <- wtd.mean(data.temp$OOP, data.temp$WGT)
  results.OOP_INS.average <- c(results.OOP_INS.average, OOP_INS_avg)
  OOP_INS_avg <- wtd.mean(data.temp$OOP * (1 - data.temp$NOINS),
                          data.temp$WGT * (1 - data.temp$NOINS))
  results.OOP_INS.average <- c(results.OOP_INS.average, OOP_INS_avg)
  OOP_INS_avg <- wtd.mean(data.temp$OOP * data.temp$NOINS,
                          data.temp$WGT * data.temp$NOINS)
  results.OOP_INS.average <- c(results.OOP_INS.average, OOP_INS_avg)
  if (year == 1996) {
    results.OOP_INS.ratio <- c(results.OOP_INS.ratio, rep(1, 3))
  } else {
    results.OOP_INS.ratio <- c(results.OOP_INS.ratio,
                               results.OOP_INS.average[(length(results.OOP_INS.average) - 2):length(results.OOP_INS.average)] /
                                 results.OOP_INS.average[1:3])
  }
}
rm(year, data.temp)
gc()
results.OOP_INS <- data.frame(
  year = as.character(results.OOP_INS.year),
  status = factor(results.OOP_INS.status),
  average = results.OOP_INS.average,
  ratio = results.OOP_INS.ratio
)
rm(
  results.OOP_INS.year,
  results.OOP_INS.status,
  results.OOP_INS.average,
  results.OOP_INS.ratio
)
gc()
par(mar = c(0, 0, 0, 0))
ggplot(data = results.OOP_INS, aes(
  x = year,
  y = average,
  group = status,
  color = status
)) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year", y = "$")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_mean_by_INS.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()
par(mar = c(0, 0, 0, 0))
ggplot(data = results.OOP_INS, aes(
  x = year,
  y = ratio,
  group = status,
  color = status
)) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year", y = "")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ratio_by_INS.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()

# Adjusted OOP -----------------------------------------------------------------
# results.ADJ_OOP_Mean <- c()
# for (year in years) {
#   results.ADJ_OOP_Mean <- c(results.ADJ_OOP_Mean,
#                             wtd.mean(MEPS_all$ADJ_OOP[MEPS_all$Year == year], MEPS_all$WGT[MEPS_all$Year == year]))
# }
# results.ADJ_OOP_Mean <- data.frame(year = factor(years), average = results.ADJ_OOP_Mean)
# par(mar = c(0, 0, 0, 0))
# ggplot(data = results.ADJ_OOP_Mean, aes(x = year, y = average, group = 1)) +
#   geom_line(color = "blue", linewidth = 1) +
#   geom_point(color = "blue", size = 3) +
#   theme(
#         legend.position = "top",  legend.key.width = unit(1.3, "cm"),
#     text = element_text(size = 18, family = "serif"),
#     legend.title = element_text(size = 15),
#     legend.text = element_text(size = 16),
#     axis.text.x = element_text(size = 13),
#     axis.text.y = element_text(size = 13)
#   ) +
#   labs(x = "year", y = "$") +
#   scale_x_discrete(breaks = seq(years[1], years[length(years)], by = 4))
# dev.copy(pdf,
#          paste0(dic_fig, "OOP_ADJ_mean.pdf"),
#          width = figure.width,
#          height = figure.height)
# dev.off()

# Adjusted OOP (Uninsured) -----------------------------------------------------
# results.ADJ_OOP_Mean_NOINS <- c()
# for (year in years) {
#   results.ADJ_OOP_Mean_NOINS <- c(
#     results.ADJ_OOP_Mean_NOINS,
#     wtd.mean(
#       MEPS_all$ADJ_OOP[MEPS_all$Year == year] * MEPS_all$NOINS[MEPS_all$Year == year],
#       MEPS_all$WGT[MEPS_all$Year == year] * MEPS_all$NOINS[MEPS_all$Year == year]
#     )
#   )
# }
# results.ADJ_OOP_Mean_NOINS <- data.frame(year = factor(years), average = results.ADJ_OOP_Mean_NOINS)
# par(mar = c(0, 0, 0, 0))
# ggplot(data = results.ADJ_OOP_Mean_NOINS, aes(x = year, y = average, group = 1)) +
#   geom_line(color = "blue", size = 1) +
#   geom_point(color = "blue", size = 3) +
#   theme(
#         legend.position = "top",  legend.key.width = unit(1.3, "cm"),
#     text = element_text(size = 18, family = "serif"),
#     legend.title = element_text(size = 15),
#     legend.text = element_text(size = 16),
#     axis.text.x = element_text(size = 13),
#     axis.text.y = element_text(size = 13)
#   ) +
#   labs(x = "year", y = "$") +
#   scale_x_discrete(breaks = seq(years[1], years[length(years)], by = 4))
# dev.copy(pdf,
#          paste0(dic_fig, "OOP_ADJ_mean_NOINS.pdf"),
#          width = figure.width,
#          height = figure.height)
# dev.off()

# Adjusted OOP by Insurance Status ---------------------------------------------
results.ADJ_OOP_INS.year <- c()
results.ADJ_OOP_INS.status <- c()
results.ADJ_OOP_INS.average <- c()
results.ADJ_OOP_INS.ratio <- c()
for (year in years) {
  results.ADJ_OOP_INS.year <- c(results.ADJ_OOP_INS.year, rep(year, 3))
  results.ADJ_OOP_INS.status <- c(results.ADJ_OOP_INS.status,
                                  c("All", "Insured", "Uninsured"))
  data.temp <- MEPS_all[MEPS_all$YEAR == year, ]
  ADJ_OOP_INS_avg <- wtd.mean(data.temp$ADJ_OOP, data.temp$WGT)
  results.ADJ_OOP_INS.average <- c(results.ADJ_OOP_INS.average, ADJ_OOP_INS_avg)
  ADJ_OOP_INS_avg <- wtd.mean(data.temp$ADJ_OOP * (1 - data.temp$NOINS),
                              data.temp$WGT * (1 - data.temp$NOINS))
  results.ADJ_OOP_INS.average <- c(results.ADJ_OOP_INS.average, ADJ_OOP_INS_avg)
  ADJ_OOP_INS_avg <- wtd.mean(data.temp$ADJ_OOP * data.temp$NOINS,
                              data.temp$WGT * data.temp$NOINS)
  results.ADJ_OOP_INS.average <- c(results.ADJ_OOP_INS.average, ADJ_OOP_INS_avg)
  if (year == 1996) {
    results.ADJ_OOP_INS.ratio <- c(results.ADJ_OOP_INS.ratio, rep(1, 3))
  } else {
    results.ADJ_OOP_INS.ratio <- c(
      results.ADJ_OOP_INS.ratio,
      results.ADJ_OOP_INS.average[(length(results.ADJ_OOP_INS.average) - 2):length(results.ADJ_OOP_INS.average)] /
        results.ADJ_OOP_INS.average[1:3]
    )
  }
}
rm(year, data.temp)
gc()
results.ADJ_OOP_INS <- data.frame(
  year = as.character(results.ADJ_OOP_INS.year),
  status = factor(results.ADJ_OOP_INS.status),
  average = results.ADJ_OOP_INS.average,
  ratio = results.ADJ_OOP_INS.ratio
)
rm(
  results.ADJ_OOP_INS.year,
  results.ADJ_OOP_INS.status,
  results.ADJ_OOP_INS.average,
  results.ADJ_OOP_INS.ratio
)
gc()
par(mar = c(0, 0, 0, 0))
ggplot(data = results.ADJ_OOP_INS, aes(
  x = year,
  y = average,
  group = status,
  color = status
)) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year", y = "$")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ADJ_mean_by_INS.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()
par(mar = c(0, 0, 0, 0))
ggplot(data = results.ADJ_OOP_INS, aes(
  x = year,
  y = ratio,
  group = status,
  color = status
)) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  ylim(c(1.0, 2.7)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year", y = "")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ADJ_ratio_by_INS.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()

# Adjusted OOP by Insurance Status and Year Group ------------------------------
results.ADJ_OOP_INS_YG.year_group <- c()
results.ADJ_OOP_INS_YG.status <- c()
results.ADJ_OOP_INS_YG.average <- c()
results.ADJ_OOP_INS_YG.ratio <- c()
for (year_group in 1:year.group.num) {
  results.ADJ_OOP_INS_YG.year_group <- c(results.ADJ_OOP_INS_YG.year_group, rep(year.group[year_group], 3))
  results.ADJ_OOP_INS_YG.status <- c(results.ADJ_OOP_INS_YG.status,
                                     c("All", "Insured", "Uninsured"))
  data.temp <- MEPS_all[MEPS_all$YEAR_G == year_group, ]
  ADJ_OOP_INS_avg <- wtd.mean(data.temp$ADJ_OOP, data.temp$WGT)
  results.ADJ_OOP_INS_YG.average <- c(results.ADJ_OOP_INS_YG.average, ADJ_OOP_INS_avg)
  ADJ_OOP_INS_avg <- wtd.mean(data.temp$ADJ_OOP * (1 - data.temp$NOINS),
                              data.temp$WGT * (1 - data.temp$NOINS))
  results.ADJ_OOP_INS_YG.average <- c(results.ADJ_OOP_INS_YG.average, ADJ_OOP_INS_avg)
  ADJ_OOP_INS_avg <- wtd.mean(data.temp$ADJ_OOP * data.temp$NOINS,
                              data.temp$WGT * data.temp$NOINS)
  results.ADJ_OOP_INS_YG.average <- c(results.ADJ_OOP_INS_YG.average, ADJ_OOP_INS_avg)
  if (year_group == 1) {
    results.ADJ_OOP_INS_YG.ratio <- c(results.ADJ_OOP_INS_YG.ratio, rep(1, 3))
  } else {
    results.ADJ_OOP_INS_YG.ratio <- c(
      results.ADJ_OOP_INS_YG.ratio,
      results.ADJ_OOP_INS_YG.average[(length(results.ADJ_OOP_INS_YG.average) - 2):length(results.ADJ_OOP_INS_YG.average)] /
        results.ADJ_OOP_INS_YG.average[1:3]
    )
  }
}
rm(year_group, data.temp)
gc()
results.ADJ_OOP_INS_YG <- data.frame(
  year_group = as.character(results.ADJ_OOP_INS_YG.year_group),
  status = factor(results.ADJ_OOP_INS_YG.status),
  average = results.ADJ_OOP_INS_YG.average,
  ratio = results.ADJ_OOP_INS_YG.ratio
)
rm(
  results.ADJ_OOP_INS_YG.year_group,
  results.ADJ_OOP_INS_YG.status,
  results.ADJ_OOP_INS_YG.average,
  results.ADJ_OOP_INS_YG.ratio
)
gc()
par(mar = c(0, 0, 0, 0))
ggplot(data = results.ADJ_OOP_INS_YG,
       aes(
         x = year_group,
         y = average,
         group = status,
         color = status
       )) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  # scale_x_discrete(breaks = year.break) +
  scale_x_discrete(labels = year.group.name) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year group", y = "$")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ADJ_mean_by_INS_YG.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()
par(mar = c(0, 0, 0, 0))
ggplot(data = results.ADJ_OOP_INS_YG, aes(
  x = year_group,
  y = ratio,
  group = status,
  color = status
)) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  # ylim(c(1.0, 2.5)) +
  # scale_x_discrete(breaks = year.break) +
  scale_x_discrete(labels = year.group.name) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year group", y = "")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ADJ_ratio_by_INS_YG.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()

# Adjusted OOP to Income by Insurance Status -----------------------------------
results.ADJ_OOP_INCOME_INS.year <- c()
results.ADJ_OOP_INCOME_INS.status <- c()
results.ADJ_OOP_INCOME_INS.average <- c()
results.ADJ_OOP_INCOME_INS.ratio <- c()
for (year in years) {
  results.ADJ_OOP_INCOME_INS.year <- c(results.ADJ_OOP_INCOME_INS.year, rep(year, 3))
  results.ADJ_OOP_INCOME_INS.status <- c(results.ADJ_OOP_INCOME_INS.status,
                                         c("All", "Insured", "Uninsured"))
  data.temp <- MEPS_all[MEPS_all$YEAR == year, ]
  INCOME_avg <- wtd.mean(data.temp$INCOME, data.temp$WGT)
  ADJ_OOP_INS_avg <- wtd.mean(data.temp$ADJ_OOP, data.temp$WGT)
  results.ADJ_OOP_INCOME_INS.average <- c(results.ADJ_OOP_INCOME_INS.average,
                                          ADJ_OOP_INS_avg / INCOME_avg)
  ADJ_OOP_INS_avg <- wtd.mean(data.temp$ADJ_OOP * (1 - data.temp$NOINS),
                              data.temp$WGT * (1 - data.temp$NOINS))
  results.ADJ_OOP_INCOME_INS.average <- c(results.ADJ_OOP_INCOME_INS.average,
                                          ADJ_OOP_INS_avg / INCOME_avg)
  ADJ_OOP_INS_avg <- wtd.mean(data.temp$ADJ_OOP * data.temp$NOINS,
                              data.temp$WGT * data.temp$NOINS)
  results.ADJ_OOP_INCOME_INS.average <- c(results.ADJ_OOP_INCOME_INS.average,
                                          ADJ_OOP_INS_avg / INCOME_avg)
  if (year == 1996) {
    results.ADJ_OOP_INCOME_INS.ratio <- c(results.ADJ_OOP_INCOME_INS.ratio, rep(1, 3))
  } else {
    results.ADJ_OOP_INCOME_INS.ratio <- c(
      results.ADJ_OOP_INCOME_INS.ratio,
      results.ADJ_OOP_INCOME_INS.average[(length(results.ADJ_OOP_INCOME_INS.average) - 2):length(results.ADJ_OOP_INCOME_INS.average)] /
        results.ADJ_OOP_INCOME_INS.average[1:3]
    )
  }
}
rm(year, data.temp, INCOME_avg, ADJ_OOP_INS_avg)
gc()
results.ADJ_OOP_INCOME_INS <- data.frame(
  year = as.character(results.ADJ_OOP_INCOME_INS.year),
  status = factor(results.ADJ_OOP_INCOME_INS.status),
  average = results.ADJ_OOP_INCOME_INS.average,
  ratio = results.ADJ_OOP_INCOME_INS.ratio
)
rm(
  results.ADJ_OOP_INCOME_INS.year,
  results.ADJ_OOP_INCOME_INS.status,
  results.ADJ_OOP_INCOME_INS.average,
  results.ADJ_OOP_INCOME_INS.ratio
)
gc()
par(mar = c(0, 0, 0, 0))
ggplot(data = results.ADJ_OOP_INCOME_INS, aes(
  x = year,
  y = average * 100,
  group = status,
  color = status
)) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year", y = "%")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ADJ_mean_to_INCOME_by_INS.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()
par(mar = c(0, 0, 0, 0))
ggplot(data = results.ADJ_OOP_INCOME_INS, aes(
  x = year,
  y = ratio,
  group = status,
  color = status
)) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year", y = "")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ADJ_ratio_to_INCOME_by_INS.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()

# Adjusted OOP to Income by Insurance Status (Median) --------------------------
results.ADJ_OOP_INCOME_MED_INS.year <- c()
results.ADJ_OOP_INCOME_MED_INS.status <- c()
results.ADJ_OOP_INCOME_MED_INS.average <- c()
results.ADJ_OOP_INCOME_MED_INS.ratio <- c()
for (year in years) {
  results.ADJ_OOP_INCOME_MED_INS.year <- c(results.ADJ_OOP_INCOME_MED_INS.year, rep(year, 3))
  results.ADJ_OOP_INCOME_MED_INS.status <- c(results.ADJ_OOP_INCOME_MED_INS.status,
                                             c("All", "Insured", "Uninsured"))
  data.temp <- MEPS_all[MEPS_all$YEAR == year, ]
  ADJ_OOP_INC_MED <- Hmisc::wtd.quantile(data.temp$ADJ_OOP_INC, data.temp$WGT, probs = c(0.5))
  results.ADJ_OOP_INCOME_MED_INS.average <- c(results.ADJ_OOP_INCOME_MED_INS.average, ADJ_OOP_INC_MED)
  
  data.temp <- MEPS_all[MEPS_all$YEAR == year &
                          MEPS_all$NOINS == 0, ]
  ADJ_OOP_INC_MED <- Hmisc::wtd.quantile(data.temp$ADJ_OOP_INC, data.temp$WGT, probs = c(0.5))
  results.ADJ_OOP_INCOME_MED_INS.average <- c(results.ADJ_OOP_INCOME_MED_INS.average, ADJ_OOP_INC_MED)
  
  data.temp <- MEPS_all[MEPS_all$YEAR == year &
                          MEPS_all$NOINS == 1, ]
  ADJ_OOP_INC_MED <- Hmisc::wtd.quantile(data.temp$ADJ_OOP_INC, data.temp$WGT, probs = c(0.5))
  results.ADJ_OOP_INCOME_MED_INS.average <- c(results.ADJ_OOP_INCOME_MED_INS.average, ADJ_OOP_INC_MED)
  
  if (year == 1996) {
    results.ADJ_OOP_INCOME_MED_INS.ratio <- c(results.ADJ_OOP_INCOME_MED_INS.ratio, rep(1, 3))
  } else {
    results.ADJ_OOP_INCOME_MED_INS.ratio <- c(
      results.ADJ_OOP_INCOME_MED_INS.ratio,
      results.ADJ_OOP_INCOME_MED_INS.average[(length(results.ADJ_OOP_INCOME_MED_INS.average) - 2):length(results.ADJ_OOP_INCOME_MED_INS.average)] /
        results.ADJ_OOP_INCOME_MED_INS.average[1:3]
    )
  }
}
rm(year, data.temp, ADJ_OOP_INC_MED)
gc()
results.ADJ_OOP_INCOME_MED_INS <- data.frame(
  year = as.character(results.ADJ_OOP_INCOME_MED_INS.year),
  status = factor(results.ADJ_OOP_INCOME_MED_INS.status),
  average = results.ADJ_OOP_INCOME_MED_INS.average,
  ratio = results.ADJ_OOP_INCOME_MED_INS.ratio
)
rm(
  results.ADJ_OOP_INCOME_MED_INS.year,
  results.ADJ_OOP_INCOME_MED_INS.status,
  results.ADJ_OOP_INCOME_MED_INS.average,
  results.ADJ_OOP_INCOME_MED_INS.ratio
)
gc()
par(mar = c(0, 0, 0, 0))
ggplot(data = results.ADJ_OOP_INCOME_MED_INS, aes(
  x = year,
  y = average * 100,
  group = status,
  color = status
)) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year", y = "%")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ADJ_to_INCOME_MED_by_INS.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()
par(mar = c(0, 0, 0, 0))
ggplot(data = results.ADJ_OOP_INCOME_MED_INS, aes(
  x = year,
  y = ratio,
  group = status,
  color = status
)) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year", y = "")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ADJ_to_INCOME_MED_ratio_by_INS.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()

# Adjusted OOP to Income by Insurance Status and Year Group --------------------
results.ADJ_OOP_INCOME_INS_YG.year_group <- c()
results.ADJ_OOP_INCOME_INS_YG.status <- c()
results.ADJ_OOP_INCOME_INS_YG.average <- c()
results.ADJ_OOP_INCOME_INS_YG.ratio <- c()
for (year_group in 1:year.group.num) {
  results.ADJ_OOP_INCOME_INS_YG.year_group <- c(results.ADJ_OOP_INCOME_INS_YG.year_group,
                                                rep(year.group[year_group], 3))
  results.ADJ_OOP_INCOME_INS_YG.status <- c(results.ADJ_OOP_INCOME_INS_YG.status,
                                            c("All", "Insured", "Uninsured"))
  data.temp <- MEPS_all[MEPS_all$YEAR_G == year_group, ]
  INCOME_avg <- wtd.mean(data.temp$INCOME, data.temp$WGT)
  ADJ_OOP_INS_avg <- wtd.mean(data.temp$ADJ_OOP, data.temp$WGT)
  results.ADJ_OOP_INCOME_INS_YG.average <- c(results.ADJ_OOP_INCOME_INS_YG.average,
                                             ADJ_OOP_INS_avg / INCOME_avg)
  ADJ_OOP_INS_avg <- wtd.mean(data.temp$ADJ_OOP * (1 - data.temp$NOINS),
                              data.temp$WGT * (1 - data.temp$NOINS))
  results.ADJ_OOP_INCOME_INS_YG.average <- c(results.ADJ_OOP_INCOME_INS_YG.average,
                                             ADJ_OOP_INS_avg / INCOME_avg)
  ADJ_OOP_INS_avg <- wtd.mean(data.temp$ADJ_OOP * data.temp$NOINS,
                              data.temp$WGT * data.temp$NOINS)
  results.ADJ_OOP_INCOME_INS_YG.average <- c(results.ADJ_OOP_INCOME_INS_YG.average,
                                             ADJ_OOP_INS_avg / INCOME_avg)
  if (year_group == 1) {
    results.ADJ_OOP_INCOME_INS_YG.ratio <- c(results.ADJ_OOP_INCOME_INS_YG.ratio, rep(1, 3))
  } else {
    results.ADJ_OOP_INCOME_INS_YG.ratio <- c(
      results.ADJ_OOP_INCOME_INS_YG.ratio,
      results.ADJ_OOP_INCOME_INS_YG.average[(length(results.ADJ_OOP_INCOME_INS_YG.average) - 2):length(results.ADJ_OOP_INCOME_INS_YG.average)] /
        results.ADJ_OOP_INCOME_INS_YG.average[1:3]
    )
  }
}
rm(year_group, data.temp, INCOME_avg, ADJ_OOP_INS_avg)
gc()
results.ADJ_OOP_INCOME_INS_YG <- data.frame(
  year_group = as.character(results.ADJ_OOP_INCOME_INS_YG.year_group),
  status = factor(results.ADJ_OOP_INCOME_INS_YG.status),
  average = results.ADJ_OOP_INCOME_INS_YG.average,
  ratio = results.ADJ_OOP_INCOME_INS_YG.ratio
)
rm(
  results.ADJ_OOP_INCOME_INS_YG.year_group,
  results.ADJ_OOP_INCOME_INS_YG.status,
  results.ADJ_OOP_INCOME_INS_YG.average,
  results.ADJ_OOP_INCOME_INS_YG.ratio
)
gc()
par(mar = c(0, 0, 0, 0))
ggplot(data = results.ADJ_OOP_INCOME_INS_YG,
       aes(
         x = year_group,
         y = average * 100,
         group = status,
         color = status
       )) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  # scale_x_discrete(breaks = year.break) +
  scale_x_discrete(labels = year.group.name) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year", y = "%")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ADJ_mean_to_INCOME_by_INS_YG.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()
par(mar = c(0, 0, 0, 0))
ggplot(data = results.ADJ_OOP_INCOME_INS_YG, aes(
  x = year_group,
  y = ratio,
  group = status,
  color = status
)) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  # scale_x_discrete(breaks = year.break) +
  scale_x_discrete(labels = year.group.name) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year", y = "")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ADJ_ratio_to_INCOME_by_INS_YG.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()

# Life Cycle

# Adjusted OOP by Year and Age Group -------------------------------------------
results.ADJ_OOP_AGE_G.year_group <- c()
results.ADJ_OOP_AGE_G.age_group <- c()
results.ADJ_OOP_AGE_G.average_by_year_group <- c()
results.ADJ_OOP_AGE_G.average <- c()
for (j in seq(age.group.num)) {
  for (i in seq(year.group.num)) {
    results.ADJ_OOP_AGE_G.year_group <- c(results.ADJ_OOP_AGE_G.year_group, year.group[i])
    results.ADJ_OOP_AGE_G.age_group <- c(results.ADJ_OOP_AGE_G.age_group, age.group[j])
    data.temp <- MEPS_all[MEPS_all$YEAR_G == i, ]
    data.temp <- data.temp[data.temp$AGE_G == j, ]
    results.ADJ_OOP_AGE_G.average_by_year_group <- c(
      results.ADJ_OOP_AGE_G.average_by_year_group,
      wtd.mean(data.temp$ADJ_OOP, data.temp$WGT)
    )
  }
  data.temp <- data.temp[data.temp$AGE_G == j, ]
  results.ADJ_OOP_AGE_G.average <- c(results.ADJ_OOP_AGE_G.average,
                                     rep(wtd.mean(data.temp$ADJ_OOP, data.temp$WGT), year.group.num))
}
rm(i, j, data.temp)
gc()
results.ADJ_OOP_AGE_G <- data.frame(
  year_group = as.character(results.ADJ_OOP_AGE_G.year_group),
  age_group = factor(results.ADJ_OOP_AGE_G.age_group),
  average_by_year_group = results.ADJ_OOP_AGE_G.average_by_year_group,
  average = results.ADJ_OOP_AGE_G.average
)
rm(
  results.ADJ_OOP_AGE_G.year_group,
  results.ADJ_OOP_AGE_G.age_group,
  results.ADJ_OOP_AGE_G.average_by_year_group,
  results.ADJ_OOP_AGE_G.average
)
gc()
par(mar = c(0, 0, 0, 0))
ggplot(
  data = results.ADJ_OOP_AGE_G,
  aes(
    x = age_group,
    y = average_by_year_group,
    group = year_group,
    color = year_group
  )
) +
  geom_line(linewidth = 1, aes(linetype = year_group)) +
  geom_point(size = 3, aes(shape = year_group)) +
  scale_x_discrete(labels = age.group) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Age group", y = "$")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ADJ_mean_by_AGE_across_YEAR.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()
results.ADJ_OOP_AGE_G_ <- data.frame(
  age_group = results.ADJ_OOP_AGE_G$age_group,
  average = results.ADJ_OOP_AGE_G$average,
  status = rep("Pooled Average", age.group.num)
)
results.ADJ_OOP_AGE_G_ <- results.ADJ_OOP_AGE_G_[!duplicated(results.ADJ_OOP_AGE_G_), ]
par(mar = c(0, 0, 0, 0))
ggplot(data = results.ADJ_OOP_AGE_G_, aes(
  x = age_group,
  y = average,
  group = status,
  color = status
)) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(labels = age.group) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Age group", y = "$")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ADJ_mean_by_AGE.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()

# Adjusted OOP to Income by Year and Age Group ---------------------------------
results.ADJ_OOP_to_INCOME_AGE_G.year_group <- c()
results.ADJ_OOP_to_INCOME_AGE_G.age_group <- c()
results.ADJ_OOP_to_INCOME_AGE_G.average_by_year_group <- c()
results.ADJ_OOP_to_INCOME_AGE_G.average <- c()
for (j in seq(age.group.num)) {
  for (i in seq(year.group.num)) {
    results.ADJ_OOP_to_INCOME_AGE_G.year_group <- c(results.ADJ_OOP_to_INCOME_AGE_G.year_group, year.group[i])
    results.ADJ_OOP_to_INCOME_AGE_G.age_group <- c(results.ADJ_OOP_to_INCOME_AGE_G.age_group, age.group[j])
    data.temp <- MEPS_all[MEPS_all$YEAR_G == i, ]
    data.temp <- data.temp[data.temp$AGE_G == j, ]
    average_by_year_group_ <- wtd.mean(data.temp$ADJ_OOP, data.temp$WGT) / wtd.mean(data.temp$INCOME, data.temp$WGT)
    results.ADJ_OOP_to_INCOME_AGE_G.average_by_year_group <- c(
      results.ADJ_OOP_to_INCOME_AGE_G.average_by_year_group,
      average_by_year_group_
    )
  }
  data.temp <- MEPS_all[MEPS_all$AGE_G == j, ]
  average_ <- wtd.mean(data.temp$ADJ_OOP, data.temp$WGT) / wtd.mean(data.temp$INCOME, data.temp$WGT)
  results.ADJ_OOP_to_INCOME_AGE_G.average <- c(results.ADJ_OOP_to_INCOME_AGE_G.average,
                                               rep(average_, year.group.num))
}
rm(i, j, data.temp, average_by_year_group_, average_)
gc()
results.ADJ_OOP_to_INCOME_AGE_G <- data.frame(
  year_group = as.character(results.ADJ_OOP_to_INCOME_AGE_G.year_group),
  age_group = factor(results.ADJ_OOP_to_INCOME_AGE_G.age_group),
  average_by_year_group = results.ADJ_OOP_to_INCOME_AGE_G.average_by_year_group * 100,
  average = results.ADJ_OOP_to_INCOME_AGE_G.average * 100
)
rm(
  results.ADJ_OOP_to_INCOME_AGE_G.year_group,
  results.ADJ_OOP_to_INCOME_AGE_G.age_group,
  results.ADJ_OOP_to_INCOME_AGE_G.average_by_year_group,
  results.ADJ_OOP_to_INCOME_AGE_G.average
)
gc()
par(mar = c(0, 0, 0, 0))
ggplot(
  data = results.ADJ_OOP_to_INCOME_AGE_G,
  aes(
    x = age_group,
    y = average_by_year_group,
    group = year_group,
    color = year_group
  )
) +
  geom_line(linewidth = 1, aes(linetype = year_group)) +
  geom_point(size = 3, aes(shape = year_group)) +
  scale_x_discrete(labels = age.group) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Age group", y = "%")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ADJ_mean_to_INCOME_by_AGE.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()

# Adjusted OOP to Income by (C)Year and Age Group ------------------------------
results.ADJ_OOP_to_INCOME_AGE_G_YEAR_CG.year_group <- c()
results.ADJ_OOP_to_INCOME_AGE_G_YEAR_CG.age_group <- c()
results.ADJ_OOP_to_INCOME_AGE_G_YEAR_CG.average_by_year_group <- c()
results.ADJ_OOP_to_INCOME_AGE_G_YEAR_CG.average <- c()
for (j in seq(age.group.num)) {
  for (i in seq(year.combined.group.num)) {
    results.ADJ_OOP_to_INCOME_AGE_G_YEAR_CG.year_group <- c(results.ADJ_OOP_to_INCOME_AGE_G_YEAR_CG.year_group,
                                                            year.combined.group[i])
    results.ADJ_OOP_to_INCOME_AGE_G_YEAR_CG.age_group <- c(results.ADJ_OOP_to_INCOME_AGE_G_YEAR_CG.age_group,
                                                           age.group[j])
    data.temp <- MEPS_all[MEPS_all$YEAR_CG == i, ]
    data.temp <- data.temp[data.temp$AGE_G == j, ]
    average_by_year_group_ <- wtd.mean(data.temp$ADJ_OOP, data.temp$WGT) / wtd.mean(data.temp$INCOME, data.temp$WGT)
    results.ADJ_OOP_to_INCOME_AGE_G_YEAR_CG.average_by_year_group <- c(
      results.ADJ_OOP_to_INCOME_AGE_G_YEAR_CG.average_by_year_group,
      average_by_year_group_
    )
  }
  data.temp <- MEPS_all[MEPS_all$AGE_G == j, ]
  average_ <- wtd.mean(data.temp$ADJ_OOP, data.temp$WGT) / wtd.mean(data.temp$INCOME, data.temp$WGT)
  results.ADJ_OOP_to_INCOME_AGE_G_YEAR_CG.average <- c(
    results.ADJ_OOP_to_INCOME_AGE_G_YEAR_CG.average,
    rep(average_, year.combined.group.num)
  )
}
rm(i, j, data.temp, average_by_year_group_, average_)
gc()
results.ADJ_OOP_to_INCOME_AGE_G_YEAR_CG <- data.frame(
  year_group = as.character(results.ADJ_OOP_to_INCOME_AGE_G_YEAR_CG.year_group),
  age_group = factor(results.ADJ_OOP_to_INCOME_AGE_G_YEAR_CG.age_group),
  average_by_year_group = results.ADJ_OOP_to_INCOME_AGE_G_YEAR_CG.average_by_year_group * 100,
  average = results.ADJ_OOP_to_INCOME_AGE_G_YEAR_CG.average * 100
)
rm(
  results.ADJ_OOP_to_INCOME_AGE_G_YEAR_CG.year_group,
  results.ADJ_OOP_to_INCOME_AGE_G_YEAR_CG.age_group,
  results.ADJ_OOP_to_INCOME_AGE_G_YEAR_CG.average_by_year_group,
  results.ADJ_OOP_to_INCOME_AGE_G_YEAR_CG.average
)
gc()
par(mar = c(0, 0, 0, 0))
ggplot(
  data = results.ADJ_OOP_to_INCOME_AGE_G_YEAR_CG,
  aes(
    x = age_group,
    y = average_by_year_group,
    group = year_group,
    color = year_group
  )
) +
  geom_line(linewidth = 1, aes(linetype = year_group)) +
  geom_point(size = 3, aes(shape = year_group)) +
  scale_x_discrete(labels = age.group) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Age group", y = "%")
dev.copy(
  pdf,
  paste0(
    getwd(),
    "/figure/OOP_ADJ_mean_to_INCOME_by_AGE_YEAR_CG.pdf"
  ),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()

# Adjusted OOP to Income by (C)Year and Income Group ------------------------------
results.ADJ_OOP_to_INCOME_INC_G_YEAR_CG.year_group <- c()
results.ADJ_OOP_to_INCOME_INC_G_YEAR_CG.age_group <- c()
results.ADJ_OOP_to_INCOME_INC_G_YEAR_CG.average_by_year_group <- c()
results.ADJ_OOP_to_INCOME_INC_G_YEAR_CG.average <- c()
for (j in seq(inc.group.num)) {
  for (i in seq(year.combined.group.num)) {
    results.ADJ_OOP_to_INCOME_INC_G_YEAR_CG.year_group <- c(results.ADJ_OOP_to_INCOME_INC_G_YEAR_CG.year_group,
                                                            year.combined.group[i])
    results.ADJ_OOP_to_INCOME_INC_G_YEAR_CG.age_group <- c(results.ADJ_OOP_to_INCOME_INC_G_YEAR_CG.age_group, j)
    data.temp <- MEPS_all[MEPS_all$YEAR_CG == i, ]
    data.temp <- data.temp[data.temp$INCOME_G == j, ]
    average_by_year_group_ <- wtd.mean(data.temp$ADJ_OOP, data.temp$WGT) / wtd.mean(data.temp$INCOME, data.temp$WGT)
    results.ADJ_OOP_to_INCOME_INC_G_YEAR_CG.average_by_year_group <- c(
      results.ADJ_OOP_to_INCOME_INC_G_YEAR_CG.average_by_year_group,
      average_by_year_group_
    )
  }
  data.temp <- MEPS_all[MEPS_all$INCOME_G == j, ]
  average_ <- wtd.mean(data.temp$ADJ_OOP, data.temp$WGT) / wtd.mean(data.temp$INCOME, data.temp$WGT)
  results.ADJ_OOP_to_INCOME_INC_G_YEAR_CG.average <- c(
    results.ADJ_OOP_to_INCOME_INC_G_YEAR_CG.average,
    rep(average_, year.combined.group.num)
  )
}
rm(i, j, data.temp, average_by_year_group_, average_)
gc()
results.ADJ_OOP_to_INCOME_INC_G_YEAR_CG <- data.frame(
  year_group = as.character(results.ADJ_OOP_to_INCOME_INC_G_YEAR_CG.year_group),
  income_group = factor(results.ADJ_OOP_to_INCOME_INC_G_YEAR_CG.age_group),
  average_by_year_group = results.ADJ_OOP_to_INCOME_INC_G_YEAR_CG.average_by_year_group * 100,
  average = results.ADJ_OOP_to_INCOME_INC_G_YEAR_CG.average * 100
)
rm(
  results.ADJ_OOP_to_INCOME_INC_G_YEAR_CG.year_group,
  results.ADJ_OOP_to_INCOME_INC_G_YEAR_CG.age_group,
  results.ADJ_OOP_to_INCOME_INC_G_YEAR_CG.average_by_year_group,
  results.ADJ_OOP_to_INCOME_INC_G_YEAR_CG.average
)
gc()
par(mar = c(0, 0, 0, 0))
ggplot(
  data = results.ADJ_OOP_to_INCOME_INC_G_YEAR_CG,
  aes(
    x = income_group,
    y = average_by_year_group,
    group = year_group,
    color = year_group
  )
) +
  geom_line(linewidth = 1, aes(linetype = year_group)) +
  geom_point(size = 3, aes(shape = year_group)) +
  # scale_x_discrete(labels = age.group) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Income group", y = "%")
dev.copy(
  pdf,
  paste0(
    getwd(),
    "/figure/OOP_ADJ_mean_to_INCOME_by_INC_YEAR_CG.pdf"
  ),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()

# Adjusted OOP to Income by Age Group and Insurance Status ---------------------
results.ADJ_OOP_to_INCOME_AGE_G_INS.age_group <- c()
results.ADJ_OOP_to_INCOME_AGE_G_INS.status <- c()
results.ADJ_OOP_to_INCOME_AGE_G_INS.average <- c()
for (j in seq(age.group.num)) {
  results.ADJ_OOP_to_INCOME_AGE_G_INS.age_group <- c(results.ADJ_OOP_to_INCOME_AGE_G_INS.age_group,
                                                     rep(age.group[j], 3))
  results.ADJ_OOP_to_INCOME_AGE_G_INS.status <- c(results.ADJ_OOP_to_INCOME_AGE_G_INS.status,
                                                  c("All", "Insured", "Uninsured"))
  data.temp <- MEPS_all[MEPS_all$AGE_G == j, ]
  INS_avg <- wtd.mean(data.temp$ADJ_OOP, data.temp$WGT) / wtd.mean(data.temp$INCOME, data.temp$WGT)
  results.ADJ_OOP_to_INCOME_AGE_G_INS.average <- c(results.ADJ_OOP_to_INCOME_AGE_G_INS.average, INS_avg)
  INS_avg <- wtd.mean(data.temp$ADJ_OOP * (1 - data.temp$NOINS),
                      data.temp$WGT * (1 - data.temp$NOINS)) /
    wtd.mean(data.temp$INCOME * (1 - data.temp$NOINS),
             data.temp$WGT * (1 - data.temp$NOINS))
  results.ADJ_OOP_to_INCOME_AGE_G_INS.average <- c(results.ADJ_OOP_to_INCOME_AGE_G_INS.average, INS_avg)
  INS_avg <- wtd.mean(data.temp$ADJ_OOP * data.temp$NOINS,
                      data.temp$WGT * data.temp$NOINS) /
    wtd.mean(data.temp$INCOME * data.temp$NOINS,
             data.temp$WGT * data.temp$NOINS)
  results.ADJ_OOP_to_INCOME_AGE_G_INS.average <- c(results.ADJ_OOP_to_INCOME_AGE_G_INS.average, INS_avg)
}
rm(j, data.temp, INS_avg)
gc()
results.ADJ_OOP_to_INCOME_AGE_G_INS <- data.frame(
  age_group = factor(results.ADJ_OOP_to_INCOME_AGE_G_INS.age_group),
  status = as.character(results.ADJ_OOP_to_INCOME_AGE_G_INS.status),
  average = results.ADJ_OOP_to_INCOME_AGE_G_INS.average * 100
)
rm(
  results.ADJ_OOP_to_INCOME_AGE_G_INS.age_group,
  results.ADJ_OOP_to_INCOME_AGE_G_INS.status,
  results.ADJ_OOP_to_INCOME_AGE_G_INS.average
)
gc()
# results.ADJ_OOP_to_INCOME_AGE_G_INS$average_ <- results.ADJ_OOP_to_INCOME_AGE_G_INS$average
# ylim_ub <- 15.0
# results.ADJ_OOP_to_INCOME_AGE_G_INS$average[results.ADJ_OOP_to_INCOME_AGE_G_INS$average > ylim_ub] <- ylim_ub
par(mar = c(0, 0, 0, 0))
ggplot(data = results.ADJ_OOP_to_INCOME_AGE_G_INS, aes(
  x = age_group,
  y = average,
  group = status,
  color = status
)) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(labels = age.group) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Age group", y = "%") # + ylim(c(0.0, ylim_ub))
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ADJ_mean_to_INCOME_by_AGE_INS.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()

# Age Group Composition --------------------------------------------------------
results.AGE_G_SHARE.year <- c()
results.AGE_G_SHARE.age <- c()
results.AGE_G_SHARE.average <- c()
for (i in seq(year.group.num)) {
  results.AGE_G_SHARE.year <- c(results.AGE_G_SHARE.year, rep(i, age.group.num))
  results.AGE_G_SHARE.age <- c(results.AGE_G_SHARE.age, age.group)
  data.temp <- MEPS_all[MEPS_all$YEAR_G == i, ]
  for (j in seq(age.group.num)) {
    data.temp.age <- data.temp[data.temp$AGE_G == j, ]
    results.AGE_G_SHARE.average <- c(results.AGE_G_SHARE.average,
                                     sum(data.temp.age$WGT) / sum(data.temp$WGT))
  }
}
rm(i, j, data.temp, data.temp.age)
gc()
results.AGE_G_SHARE <- data.frame(
  year = as.character(results.AGE_G_SHARE.year),
  age = factor(results.AGE_G_SHARE.age),
  average = results.AGE_G_SHARE.average
)
rm(results.AGE_G_SHARE.year,
   results.AGE_G_SHARE.age,
   results.AGE_G_SHARE.average)
gc()
par(mar = c(0, 0, 0, 0))
ggplot(results.AGE_G_SHARE, aes(fill = age, y = average, x = year)) +
  geom_bar(position = "fill", stat = "identity") +
  geom_bar_pattern(
    stat = "identity",
    pattern_color = "white",
    pattern_fill = "black",
    pattern_alpha = 0.5,
    pattern_density = 0.10,
    pattern_spacing = 0.03,
    aes(pattern = age, pattern_angle = age)
  ) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    # c(0.11, 0.71),
    legend.box = "horizontal",
    ,
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year group", y = "%") +
  scale_x_discrete(labels = year.group.name)
dev.copy(
  pdf,
  paste0(getwd(), "/figure/AGE_G_Composition.pdf"),
  width = figure.width,
  height = figure.height + 1.0
)
dev.off()

# Age Group Composition (Combined) ---------------------------------------------
results.AGE_CG_SHARE.year <- c()
results.AGE_CG_SHARE.age <- c()
results.AGE_CG_SHARE.average <- c()
for (i in seq(year.group.num)) {
  results.AGE_CG_SHARE.year <- c(results.AGE_CG_SHARE.year, rep(i, age.combined.group.num))
  results.AGE_CG_SHARE.age <- c(results.AGE_CG_SHARE.age, age.combined.group)
  data.temp <- MEPS_all[MEPS_all$YEAR_G == i, ]
  for (j in seq(age.combined.group.num)) {
    data.temp.age <- data.temp[data.temp$AGE_CG == j, ]
    results.AGE_CG_SHARE.average <- c(results.AGE_CG_SHARE.average,
                                      sum(data.temp.age$WGT) / sum(data.temp$WGT))
  }
}
rm(i, j, data.temp, data.temp.age)
gc()
results.AGE_CG_SHARE <- data.frame(
  year = as.character(results.AGE_CG_SHARE.year),
  age = factor(results.AGE_CG_SHARE.age),
  average = results.AGE_CG_SHARE.average
)
rm(
  results.AGE_CG_SHARE.year,
  results.AGE_CG_SHARE.age,
  results.AGE_CG_SHARE.average
)
gc()
par(mar = c(0, 0, 0, 0))
ggplot(results.AGE_CG_SHARE, aes(fill = age, y = average, x = year)) +
  geom_bar(position = "fill", stat = "identity") +
  geom_bar_pattern(
    stat = "identity",
    pattern_color = "white",
    pattern_fill = "black",
    pattern_alpha = 0.5,
    pattern_density = 0.10,
    pattern_spacing = 0.03,
    aes(pattern = age, pattern_angle = age)
  ) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    # c(0.112, 0.80),
    legend.box = "horizontal",
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year group", y = "%") +
  scale_x_discrete(labels = year.group.name)
dev.copy(
  pdf,
  paste0(getwd(), "/figure/AGE_CG_Composition.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()

# Adjusted OOP by Quantiles ----------------------------------------------------
results.ADJ_OOP_IQR.year <- c()
results.ADJ_OOP_IQR.status <- c()
results.ADJ_OOP_IQR.average <- c()
results.ADJ_OOP_IQR_DIFF.year <- c()
results.ADJ_OOP_IQR_DIFF.status <- c()
results.ADJ_OOP_IQR_DIFF.average <- c()
for (year in years) {
  results.ADJ_OOP_IQR.year <- c(results.ADJ_OOP_IQR.year, rep(year, 3))
  results.ADJ_OOP_IQR.status <- c(results.ADJ_OOP_IQR.status, c("Q1", "Q2 (Median)", "Q3"))
  data.temp <- MEPS_all[MEPS_all$YEAR == year, ]
  quatiles.temp <- Hmisc::wtd.quantile(data.temp$ADJ_OOP,
                                       weights = data.temp$WGT,
                                       probs = c(.25, .5, .75))
  results.ADJ_OOP_IQR.average <- c(results.ADJ_OOP_IQR.average, quatiles.temp)
  
  results.ADJ_OOP_IQR_DIFF.year <- c(results.ADJ_OOP_IQR_DIFF.year, rep(year, 2))
  results.ADJ_OOP_IQR_DIFF.status <- c(results.ADJ_OOP_IQR_DIFF.status, c("IQR", "MAD"))
  results.ADJ_OOP_IQR_DIFF.average <- c(results.ADJ_OOP_IQR_DIFF.average,
                                        quatiles.temp[3] - quatiles.temp[1])
  results.ADJ_OOP_IQR_DIFF.average <- c(
    results.ADJ_OOP_IQR_DIFF.average,
    weightedMad(data.temp$ADJ_OOP, w = data.temp$WGT)
  )
}
rm(year, data.temp, quatiles.temp)
gc()
results.ADJ_OOP_IQR <- data.frame(
  year = as.character(results.ADJ_OOP_IQR.year),
  status = factor(results.ADJ_OOP_IQR.status),
  average = results.ADJ_OOP_IQR.average
)
results.ADJ_OOP_IQR_DIFF <- data.frame(
  year = as.character(results.ADJ_OOP_IQR_DIFF.year),
  status = factor(results.ADJ_OOP_IQR_DIFF.status),
  average = results.ADJ_OOP_IQR_DIFF.average
)
rm(
  results.ADJ_OOP_IQR.year,
  results.ADJ_OOP_IQR.status,
  results.ADJ_OOP_IQR.average,
  results.ADJ_OOP_IQR_DIFF.year,
  results.ADJ_OOP_IQR_DIFF.status,
  results.ADJ_OOP_IQR_DIFF.average
)
gc()
par(mar = c(0, 0, 0, 0))
ggplot(data = results.ADJ_OOP_IQR, aes(
  x = year,
  y = average,
  group = status,
  color = status
)) +
  geom_line(size = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year", y = "$")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ADJ_mean_by_IQR.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()
par(mar = c(0, 0, 0, 0))
ggplot(data = results.ADJ_OOP_IQR_DIFF, aes(
  x = year,
  y = average,
  group = status,
  color = status
)) +
  geom_line(size = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year", y = "$")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ADJ_mean_by_IQR_DIFF.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()

# Adjusted OOP by Quantiles and Insurance Status -------------------------------
results.ADJ_OOP_IQR_NOINS.year <- c()
results.ADJ_OOP_IQR_NOINS.status <- c()
results.ADJ_OOP_IQR_NOINS.moment <- c()
results.ADJ_OOP_IQR_NOINS.average <- c()
results.ADJ_OOP_IQR_NOINS_DIFF.year <- c()
results.ADJ_OOP_IQR_NOINS_DIFF.status <- c()
results.ADJ_OOP_IQR_NOINS_DIFF.moment <- c()
results.ADJ_OOP_IQR_NOINS_DIFF.average <- c()
for (year in years) {
  results.ADJ_OOP_IQR_NOINS.year <- c(results.ADJ_OOP_IQR_NOINS.year, rep(year, 6))
  results.ADJ_OOP_IQR_NOINS.status <- c(results.ADJ_OOP_IQR_NOINS.status, rep(c("All", "Uninsured"), each = 3))
  results.ADJ_OOP_IQR_NOINS.moment <- c(results.ADJ_OOP_IQR_NOINS.moment, rep(c("Q1", "Q2 (Median)", "Q3"), 2))
  data.temp <- MEPS_all[MEPS_all$YEAR == year, ]
  quatiles.temp <- Hmisc::wtd.quantile(data.temp$ADJ_OOP,
                                       weights = data.temp$WGT,
                                       probs = c(.25, .5, .75))
  results.ADJ_OOP_IQR_NOINS.average <- c(results.ADJ_OOP_IQR_NOINS.average, quatiles.temp)
  quatiles.temp.NOINS <- Hmisc::wtd.quantile(
    data.temp$ADJ_OOP * data.temp$NOINS,
    weights = data.temp$WGT * data.temp$NOINS,
    probs = c(.25, .5, .75)
  )
  results.ADJ_OOP_IQR_NOINS.average <- c(results.ADJ_OOP_IQR_NOINS.average, quatiles.temp.NOINS)
  
  
  results.ADJ_OOP_IQR_NOINS_DIFF.year <- c(results.ADJ_OOP_IQR_NOINS_DIFF.year, rep(year, 4))
  results.ADJ_OOP_IQR_NOINS_DIFF.status <- c(results.ADJ_OOP_IQR_NOINS_DIFF.status, rep(c("All", "Uninsured"), each = 2))
  results.ADJ_OOP_IQR_NOINS_DIFF.moment <- c(results.ADJ_OOP_IQR_NOINS_DIFF.moment, rep(c("IQR", "MAD"), 2))
  results.ADJ_OOP_IQR_NOINS_DIFF.average <- c(results.ADJ_OOP_IQR_NOINS_DIFF.average,
                                              quatiles.temp[3] - quatiles.temp[1])
  results.ADJ_OOP_IQR_NOINS_DIFF.average <- c(
    results.ADJ_OOP_IQR_NOINS_DIFF.average,
    weightedMad(data.temp$ADJ_OOP, w = data.temp$WGT)
  )
  results.ADJ_OOP_IQR_NOINS_DIFF.average <- c(
    results.ADJ_OOP_IQR_NOINS_DIFF.average,
    quatiles.temp.NOINS[3] - quatiles.temp.NOINS[1]
  )
  results.ADJ_OOP_IQR_NOINS_DIFF.average <- c(
    results.ADJ_OOP_IQR_NOINS_DIFF.average,
    weightedMad(data.temp$ADJ_OOP * data.temp$NOINS, w = data.temp$WGT * data.temp$NOINS)
  )
}
rm(year, data.temp, quatiles.temp, quatiles.temp.NOINS)
gc()
results.ADJ_OOP_IQR_NOINS <- data.frame(
  year = as.character(results.ADJ_OOP_IQR_NOINS.year),
  status = factor(results.ADJ_OOP_IQR_NOINS.status),
  moment = factor(results.ADJ_OOP_IQR_NOINS.moment),
  average = results.ADJ_OOP_IQR_NOINS.average
)
results.ADJ_OOP_IQR_NOINS_DIFF <- data.frame(
  year = as.character(results.ADJ_OOP_IQR_NOINS_DIFF.year),
  status = factor(results.ADJ_OOP_IQR_NOINS_DIFF.status),
  moment = factor(results.ADJ_OOP_IQR_NOINS_DIFF.moment),
  average = results.ADJ_OOP_IQR_NOINS_DIFF.average
)
rm(
  results.ADJ_OOP_IQR_NOINS.year,
  results.ADJ_OOP_IQR_NOINS.status,
  results.ADJ_OOP_IQR_NOINS.moment,
  results.ADJ_OOP_IQR_NOINS.average,
  results.ADJ_OOP_IQR_NOINS_DIFF.year,
  results.ADJ_OOP_IQR_NOINS_DIFF.status,
  results.ADJ_OOP_IQR_NOINS_DIFF.moment,
  results.ADJ_OOP_IQR_NOINS_DIFF.average
)
gc()
par(mar = c(0, 0, 0, 0))
ggplot(data = results.ADJ_OOP_IQR_NOINS,
       aes(
         x = year,
         y = average,
         color = moment,
         shape = status,
         group = interaction(status, moment),
       )) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year", y = "$")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ADJ_mean_by_IQR_NOINS.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()
par(mar = c(0, 0, 0, 0))
ggplot(data = results.ADJ_OOP_IQR_NOINS_DIFF,
       aes(
         x = year,
         y = average,
         color = moment,
         shape = status,
         group = interaction(status, moment),
       )) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year", y = "$")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ADJ_mean_by_IQR_NOINS_DIFF.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()

# Adjusted OOP by Quantiles (Unit-Free) ----------------------------------------
results.ADJ_OOP_IQR_DIFF_ADJ.year <- c()
results.ADJ_OOP_IQR_DIFF_ADJ.status <- c()
results.ADJ_OOP_IQR_DIFF_ADJ.average <- c()
for (year in years) {
  results.ADJ_OOP_IQR_DIFF_ADJ.year <- c(results.ADJ_OOP_IQR_DIFF_ADJ.year, rep(year, 2))
  results.ADJ_OOP_IQR_DIFF_ADJ.status <- c(results.ADJ_OOP_IQR_DIFF_ADJ.status, c("IQCD", "MAD(%)"))
  data.temp <- MEPS_all[MEPS_all$YEAR == year, ]
  quatiles.temp <- Hmisc::wtd.quantile(data.temp$ADJ_OOP,
                                       weights = data.temp$WGT,
                                       probs = c(.25, .5, .75))
  results.ADJ_OOP_IQR_DIFF_ADJ.average <- c(
    results.ADJ_OOP_IQR_DIFF_ADJ.average,
    (quatiles.temp[3] - quatiles.temp[1]) / (quatiles.temp[3] + quatiles.temp[1])
  )
  mad.temp <- weightedMad(data.temp$ADJ_OOP, w = data.temp$WGT)
  results.ADJ_OOP_IQR_DIFF_ADJ.average <- c(results.ADJ_OOP_IQR_DIFF_ADJ.average,
                                            mad.temp / quatiles.temp[2])
}
rm(year, data.temp, quatiles.temp, mad.temp)
gc()
results.ADJ_OOP_IQR_DIFF_ADJ <- data.frame(
  year = as.character(results.ADJ_OOP_IQR_DIFF_ADJ.year),
  status = factor(results.ADJ_OOP_IQR_DIFF_ADJ.status),
  average = results.ADJ_OOP_IQR_DIFF_ADJ.average
)
rm(
  results.ADJ_OOP_IQR_DIFF_ADJ.year,
  results.ADJ_OOP_IQR_DIFF_ADJ.status,
  results.ADJ_OOP_IQR_DIFF_ADJ.average
)
gc()
par(mar = c(0, 0, 0, 0))
ggplot(data = results.ADJ_OOP_IQR_DIFF_ADJ[results.ADJ_OOP_IQR_DIFF_ADJ$status == "IQCD", ], aes(
  x = year,
  y = average,
  group = status,
  color = status
)) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year", y = "$")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ADJ_mean_by_IQR_ADJ.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()

# Adjusted OOP to Income by Quantiles and Insurance Status ---------------------
results.ADJ_OOP_INC_IQR_INS.year <- c()
results.ADJ_OOP_INC_IQR_INS.status <- c()
results.ADJ_OOP_INC_IQR_INS.average <- c()
results.ADJ_OOP_INC_IQR_INS.ratio <- c()
results.ADJ_OOP_INC_MAD_INS.year <- c()
results.ADJ_OOP_INC_MAD_INS.status <- c()
results.ADJ_OOP_INC_MAD_INS.average <- c()
for (year in years) {
  results.ADJ_OOP_INC_IQR_INS.year <- c(results.ADJ_OOP_INC_IQR_INS.year, rep(year, 3))
  results.ADJ_OOP_INC_MAD_INS.year <- c(results.ADJ_OOP_INC_MAD_INS.year, rep(year, 3))
  
  results.ADJ_OOP_INC_IQR_INS.status <- c(results.ADJ_OOP_INC_IQR_INS.status,
                                          c("All", "Insured", "Uninsured"))
  results.ADJ_OOP_INC_MAD_INS.status <- c(results.ADJ_OOP_INC_MAD_INS.status,
                                          c("All", "Insured", "Uninsured"))
  
  data.temp <- MEPS_all[MEPS_all$YEAR == year, ]
  quatiles.temp <- Hmisc::wtd.quantile(data.temp$ADJ_OOP_INC,
                                       weights = data.temp$WGT,
                                       probs = c(.25, .5, .75))
  results.ADJ_OOP_INC_IQR_INS.average <- c(results.ADJ_OOP_INC_IQR_INS.average,
                                           quatiles.temp[3] - quatiles.temp[1])
  results.ADJ_OOP_INC_MAD_INS.average <- c(
    results.ADJ_OOP_INC_MAD_INS.average,
    weightedMad(data.temp$ADJ_OOP_INC, w = data.temp$WGT)
  )
  
  data.temp <- MEPS_all[(MEPS_all$YEAR == year &
                           MEPS_all$NOINS == 0), ]
  quatiles.temp <- Hmisc::wtd.quantile(data.temp$ADJ_OOP_INC,
                                       weights = data.temp$WGT,
                                       probs = c(.25, .5, .75))
  results.ADJ_OOP_INC_IQR_INS.average <- c(results.ADJ_OOP_INC_IQR_INS.average,
                                           quatiles.temp[3] - quatiles.temp[1])
  results.ADJ_OOP_INC_MAD_INS.average <- c(
    results.ADJ_OOP_INC_MAD_INS.average,
    weightedMad(data.temp$ADJ_OOP_INC, w = data.temp$WGT)
  )
  
  data.temp <- MEPS_all[(MEPS_all$YEAR == year &
                           MEPS_all$NOINS == 1), ]
  quatiles.temp <- Hmisc::wtd.quantile(data.temp$ADJ_OOP_INC,
                                       weights = data.temp$WGT,
                                       probs = c(.25, .5, .75))
  results.ADJ_OOP_INC_IQR_INS.average <- c(results.ADJ_OOP_INC_IQR_INS.average,
                                           quatiles.temp[3] - quatiles.temp[1])
  results.ADJ_OOP_INC_MAD_INS.average <- c(
    results.ADJ_OOP_INC_MAD_INS.average,
    weightedMad(data.temp$ADJ_OOP_INC, w = data.temp$WGT)
  )
  
  if (year == 1996) {
    results.ADJ_OOP_INC_IQR_INS.ratio <- c(results.ADJ_OOP_INC_IQR_INS.ratio, rep(1, 3))
  } else {
    results.ADJ_OOP_INC_IQR_INS.ratio <- c(
      results.ADJ_OOP_INC_IQR_INS.ratio,
      results.ADJ_OOP_INC_IQR_INS.average[(length(results.ADJ_OOP_INC_IQR_INS.average) - 2):length(results.ADJ_OOP_INC_IQR_INS.average)] /
        results.ADJ_OOP_INC_IQR_INS.average[1:3]
    )
  }
}
rm(year, data.temp, quatiles.temp)
gc()
results.ADJ_OOP_INC_IQR_INS <- data.frame(
  year = as.character(results.ADJ_OOP_INC_IQR_INS.year),
  status = factor(results.ADJ_OOP_INC_IQR_INS.status),
  average = results.ADJ_OOP_INC_IQR_INS.average * 100,
  ratio = results.ADJ_OOP_INC_IQR_INS.ratio
)
results.ADJ_OOP_INC_MAD_INS <- data.frame(
  year = as.character(results.ADJ_OOP_INC_MAD_INS.year),
  status = factor(results.ADJ_OOP_INC_MAD_INS.status),
  average = results.ADJ_OOP_INC_MAD_INS.average * 100
)
rm(
  results.ADJ_OOP_INC_IQR_INS.year,
  results.ADJ_OOP_INC_IQR_INS.status,
  results.ADJ_OOP_INC_IQR_INS.average,
  results.ADJ_OOP_INC_IQR_INS.ratio,
  results.ADJ_OOP_INC_MAD_INS.year,
  results.ADJ_OOP_INC_MAD_INS.status,
  results.ADJ_OOP_INC_MAD_INS.average
)
gc()
par(mar = c(0, 0, 0, 0))
ggplot(data = results.ADJ_OOP_INC_IQR_INS, aes(
  x = year,
  y = average,
  color = status,
  group = status,
)) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year", y = "%")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ADJ_INC_by_IQR_INS.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()
par(mar = c(0, 0, 0, 0))
ggplot(data = results.ADJ_OOP_INC_IQR_INS, aes(
  x = year,
  y = ratio,
  color = status,
  group = status,
)) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year", y = "")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ADJ_INC_ratio_by_IQR_INS.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()
par(mar = c(0, 0, 0, 0))
ggplot(data = results.ADJ_OOP_INC_MAD_INS, aes(
  x = year,
  y = average,
  color = status,
  group = status,
)) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year", y = "%")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ADJ_INC_by_MAD_INS.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()

# Adjusted OOP to Income by Quantiles, Year, and Age Group ---------------------
results.ADJ_OOP_to_INCOME_IQR_AGE_G.year_group <- c()
results.ADJ_OOP_to_INCOME_IQR_AGE_G.age_group <- c()
results.ADJ_OOP_to_INCOME_IQR_AGE_G.average_by_year_group <- c()
results.ADJ_OOP_to_INCOME_IQR_AGE_G.average <- c()
for (j in seq(age.group.num)) {
  for (i in seq(year.combined.group.num)) {
    results.ADJ_OOP_to_INCOME_IQR_AGE_G.year_group <- c(results.ADJ_OOP_to_INCOME_IQR_AGE_G.year_group,
                                                        year.combined.group[i])
    results.ADJ_OOP_to_INCOME_IQR_AGE_G.age_group <- c(results.ADJ_OOP_to_INCOME_IQR_AGE_G.age_group,
                                                       age.group[j])
    data.temp <- MEPS_all[MEPS_all$YEAR_CG == i, ]
    data.temp <- data.temp[data.temp$AGE_G == j, ]
    quatiles.temp.year <- Hmisc::wtd.quantile(data.temp$ADJ_OOP_INC,
                                              weights = data.temp$WGT,
                                              probs = c(.25, .75))
    results.ADJ_OOP_to_INCOME_IQR_AGE_G.average_by_year_group <- c(
      results.ADJ_OOP_to_INCOME_IQR_AGE_G.average_by_year_group,
      quatiles.temp.year[2] - quatiles.temp.year[1]
    )
  }
  data.temp <- MEPS_all[MEPS_all$AGE_G == j, ]
  quatiles.temp <- Hmisc::wtd.quantile(data.temp$ADJ_OOP_INC,
                                       weights = data.temp$WGT,
                                       probs = c(.25, .75))
  results.ADJ_OOP_to_INCOME_IQR_AGE_G.average <- c(
    results.ADJ_OOP_to_INCOME_IQR_AGE_G.average,
    rep(quatiles.temp[2] - quatiles.temp[1], year.group.num)
  )
}
rm(i, j, data.temp, quatiles.temp.year, quatiles.temp)
gc()
results.ADJ_OOP_to_INCOME_IQR_AGE_G <- data.frame(
  year_group = as.character(results.ADJ_OOP_to_INCOME_IQR_AGE_G.year_group),
  age_group = factor(results.ADJ_OOP_to_INCOME_IQR_AGE_G.age_group),
  average_by_year_group = results.ADJ_OOP_to_INCOME_IQR_AGE_G.average_by_year_group * 100,
  average = results.ADJ_OOP_to_INCOME_IQR_AGE_G.average * 100
)
rm(
  results.ADJ_OOP_to_INCOME_IQR_AGE_G.year_group,
  results.ADJ_OOP_to_INCOME_IQR_AGE_G.age_group,
  results.ADJ_OOP_to_INCOME_IQR_AGE_G.average_by_year_group,
  results.ADJ_OOP_to_INCOME_IQR_AGE_G.average
)
gc()
par(mar = c(0, 0, 0, 0))
ggplot(
  data = results.ADJ_OOP_to_INCOME_IQR_AGE_G,
  aes(
    x = age_group,
    y = average_by_year_group,
    group = year_group,
    color = year_group
  )
) +
  geom_line(linewidth = 1, aes(linetype = year_group)) +
  geom_point(size = 3, aes(shape = year_group)) +
  scale_x_discrete(labels = age.group) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Age group", y = "%")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/OOP_ADJ_mean_to_INCOME_IQR_by_AGE.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()

# Adjusted OOP to Income by Quantiles, Age, Insurance Group --------------------
results.ADJ_OOP_to_INCOME_IQR_AGE_G_INS.age_group <- c()
results.ADJ_OOP_to_INCOME_IQR_AGE_G_INS.status <- c()
results.ADJ_OOP_to_INCOME_IQR_AGE_G_INS.average <- c()
for (j in seq(age.group.num)) {
  results.ADJ_OOP_to_INCOME_IQR_AGE_G_INS.age_group <- c(results.ADJ_OOP_to_INCOME_IQR_AGE_G_INS.age_group,
                                                         rep(age.group[j], 3))
  results.ADJ_OOP_to_INCOME_IQR_AGE_G_INS.status <- c(
    results.ADJ_OOP_to_INCOME_IQR_AGE_G_INS.status,
    c("All", "Insured", "Uninsured")
  )
  data.temp <- MEPS_all[MEPS_all$AGE_G == j, ]
  quatiles.temp <- Hmisc::wtd.quantile(data.temp$ADJ_OOP_INC,
                                       weights = data.temp$WGT,
                                       probs = c(.25, .75))
  results.ADJ_OOP_to_INCOME_IQR_AGE_G_INS.average <- c(
    results.ADJ_OOP_to_INCOME_IQR_AGE_G_INS.average,
    quatiles.temp[2] - quatiles.temp[1]
  )
  data.temp <- MEPS_all[MEPS_all$AGE_G == j & MEPS_all$NOINS == 0, ]
  quatiles.temp <- Hmisc::wtd.quantile(data.temp$ADJ_OOP_INC,
                                       weights = data.temp$WGT,
                                       probs = c(.25, .75))
  results.ADJ_OOP_to_INCOME_IQR_AGE_G_INS.average <- c(
    results.ADJ_OOP_to_INCOME_IQR_AGE_G_INS.average,
    quatiles.temp[2] - quatiles.temp[1]
  )
  data.temp <- MEPS_all[MEPS_all$AGE_G == j & MEPS_all$NOINS == 1, ]
  quatiles.temp <- Hmisc::wtd.quantile(data.temp$ADJ_OOP_INC,
                                       weights = data.temp$WGT,
                                       probs = c(.25, .75))
  results.ADJ_OOP_to_INCOME_IQR_AGE_G_INS.average <- c(
    results.ADJ_OOP_to_INCOME_IQR_AGE_G_INS.average,
    quatiles.temp[2] - quatiles.temp[1]
  )
}
rm(j, data.temp, quatiles.temp)
gc()
results.ADJ_OOP_to_INCOME_IQR_AGE_G_INS <- data.frame(
  age_group = factor(results.ADJ_OOP_to_INCOME_IQR_AGE_G_INS.age_group),
  status = as.character(results.ADJ_OOP_to_INCOME_IQR_AGE_G_INS.status),
  average = results.ADJ_OOP_to_INCOME_IQR_AGE_G_INS.average * 100
)
rm(
  results.ADJ_OOP_to_INCOME_IQR_AGE_G_INS.age_group,
  results.ADJ_OOP_to_INCOME_IQR_AGE_G_INS.status,
  results.ADJ_OOP_to_INCOME_IQR_AGE_G_INS.average
)
gc()
par(mar = c(0, 0, 0, 0))
ggplot(data = results.ADJ_OOP_to_INCOME_IQR_AGE_G_INS, aes(
  x = age_group,
  y = average,
  group = status,
  color = status
)) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(labels = age.group) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Age group", y = "%")
dev.copy(
  pdf,
  paste0(
    getwd(),
    "/figure/OOP_ADJ_mean_to_INCOME_IQR_by_AGE_INS.pdf"
  ),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()

# Employment Cost Index --------------------------------------------------------
ECI_value <- c(
  100,
  103.90986,
  107.43959,
  111.56666,
  115.82949,
  119.44067,
  122.183,
  124.19223,
  126.17431,
  128.88949,
  131.06163,
  133.26093,
  135.84035,
  138.36546,
  141.10779,
  144.77328,
  148.43877
)
ECI_Year <- c(2003:2019)
ECI_status <- rep("Employment Cost Index", length(ECI_Year))
results.ECI <- data.frame(year = ECI_Year,
                          value = ECI_value / 100,
                          status = ECI_status)
rm(ECI_Year, ECI_value, ECI_status)
par(mar = c(0, 0, 0, 0))
ggplot(data = results.ECI, aes(
  x = year,
  y = value,
  group = status,
  color = status
)) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  ylim(c(1.0, 2.7)) +
  scale_x_continuous(breaks = year.break, lim = c(years[1], years[length(years)])) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year", y = "")
dev.copy(pdf,
         paste0(getwd(), "/figure/ECI.pdf"),
         width = figure.width,
         height = figure.height + 0.5)
dev.off()

# PCE-Health -------------------------------------------------------------------
PCE_Health <- c(
  1.00,
  1.02,
  1.04,
  1.06,
  1.10,
  1.13,
  1.17,
  1.21,
  1.25,
  1.29,
  1.33,
  1.37,
  1.41,
  1.45,
  1.48,
  1.51,
  1.54,
  1.56,
  1.59,
  1.60,
  1.63,
  1.65,
  1.68,
  1.71
)
PCE_Health_status <- rep("PCE-Health", length(years))
results.PCE_Health <- data.frame(year = years,
                                 value = PCE_Health,
                                 status = PCE_Health_status)
rm(PCE_Health, PCE_Health_status)
par(mar = c(0, 0, 0, 0))
ggplot(data = results.PCE_Health, aes(
  x = year,
  y = value,
  group = status,
  color = status
)) +
  geom_line(linewidth = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_continuous(breaks = year.break, lim = c(years[1], years[length(years)])) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.3, "cm"),
    text = element_text(size = 18, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  ) +
  labs(x = "Year", y = "")
dev.copy(
  pdf,
  paste0(getwd(), "/figure/PCE_Health.pdf"),
  width = figure.width,
  height = figure.height + 0.5
)
dev.off()