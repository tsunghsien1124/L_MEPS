# Load necessary libraries
rm(list = ls())
library(Hmisc)
library(matrixStats)
library(ggplot2)
theme_set(theme_bw())
library(ggpattern)
library(viridis)

# Data directory----------------------------------------------------------------
username <- "user" # "Tsung-Hsien Li"
setwd(paste0(
  "C:/Users/",
  username,
  "/Desktop/Medical Uncertainty 1996-2020/All Year/"
))
dic_data_raw <- paste0(getwd(), "/Raw Data/")
dic_data_adj <- paste0(getwd(), "/Raw Data (Adjusted)/")
dic_fig <- paste0(getwd(), "/Figure/")

# Load MEPS data ---------------------------------------------------------------
MEPS_all <- readRDS(paste0(dic_data_adj, "MEPS_all.RData"))
MEPS_all <- MEPS_all[MEPS_all$INCOME > 0, ]
MEPS_all <- MEPS_all[MEPS_all$AGE >= 20, ]

# Create auxiliary variables ---------------------------------------------------
figure.width <- 6
figure.height <- 4.0
years <- 1996:2019
year.break <- seq(years[1], years[length(years)], by = 4)
year.group <- c("1996-2000",
                "2000-2004",
                "2004-2008",
                "2008-2012",
                "2012-2016",
                "2016-2019")
year.group.name <- c("96-00", "00-04", "04-08", "08-12", "12-16", "16-19")
# year.group <- c("1996-2004",
#                 "2004-2012",
#                 "2012-2019")
year.group.num <- length(year.group)
age.group <- c("20-35", "35-44", "45-54", "55-64", "65-74", "75+")
age.group.num <- length(age.group)
age.combined.group <- c("20-44", "45-64", "65+")
age.combined.group.num <- length(age.combined.group)

# Create auxiliary functions ---------------------------------------------------
wtd.quantile.group <- function(x, w, n) {
  results <- c()
  for (i in seq(n + 1)) {
    if (i == 1) {
      results <- c(results, -Inf)
    } else if (i == n + 1) {
      results <- c(results, Inf)
    } else {
      results <- c(results, wtd.quantile(x, q = (i - 1) / n, weight = w))
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
  breaks = c(1996, 2000, 2004, 2008, 2012, 2016, Inf),
  labels = c('1', '2', '3', '4', '5', '6'),
  # breaks = c(1996, 2004, 2012, Inf),
  # labels = c('1', '2', '3'),
  include.lowest = TRUE
)
MEPS_all$AGE_G <- cut(
  MEPS_all$AGE,
  breaks = c(20, 35, 44, 54, 64, 74, Inf),
  labels = c('1', '2', '3', '4', '5', '6'),
  include.lowest = TRUE
)
MEPS_all$AGE_CG <- cut(
  MEPS_all$AGE,
  breaks = c(20, 44, 64, Inf),
  labels = c('1', '2', '3'),
  include.lowest = TRUE
)

# Share of Uninsured -----------------------------------------------------------
results.NOINS.year <- c()
results.NOINS.status <- c()
results.NOINS.average <- c()
for (year in years) {
  results.NOINS.year <- c(results.NOINS.year, rep(year, 2))
  results.NOINS.status <- c(results.NOINS.status, c("Any month", "All year"))
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
  average = results.NOINS.average
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
  geom_line(size = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  theme(
    legend.position = "top",
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

# OOP --------------------------------------------------------------------------
# results.OOP_Mean <- c()
# for (year in years) {
#   results.OOP_Mean <- c(results.OOP_Mean, wtd.mean(MEPS_all$OOP[MEPS_all$Year == year], MEPS_all$WGT[MEPS_all$Year == year]))
# }
# results.OOP_Mean <- data.frame(year = factor(years), average = results.OOP_Mean)
# par(mar = c(0, 0, 0, 0))
# ggplot(data = results.OOP_Mean, aes(x = year, y = average, group = 1)) +
#   geom_line(color = "blue", size = 1) +
#   geom_point(color = "blue", size = 3) +
#   theme(
#     legend.position = "top",
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
  geom_line(size = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
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
  geom_line(size = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
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
#   geom_line(color = "blue", size = 1) +
#   geom_point(color = "blue", size = 3) +
#   theme(
#     legend.position = "top",
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
#     legend.position = "top",
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
  geom_line(size = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
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
  geom_line(size = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  ylim(c(1.0, 2.7)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
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
  geom_line(size = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  ylim(c(1.0, 2.7)) +
  scale_x_continuous(breaks = year.break, lim = c(years[1], years[length(years)])) +
  theme(
    legend.position = "top",
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

# Adjusted OOP by Year and Age Group -------------------------------------------
results.ADJ_OOP_AGE_G.year <- c()
results.ADJ_OOP_AGE_G.age <- c()
results.ADJ_OOP_AGE_G.average <- c()
for (i in seq(year.group.num)) {
  results.ADJ_OOP_AGE_G.year <- c(results.ADJ_OOP_AGE_G.year,
                                  rep(year.group[i], age.group.num))
  results.ADJ_OOP_AGE_G.age <- c(results.ADJ_OOP_AGE_G.age, 1:age.group.num)
  for (j in seq(age.group.num)) {
    data.temp <- MEPS_all[MEPS_all$YEAR_G == i, ]
    data.temp <- data.temp[data.temp$AGE_G == j, ]
    results.ADJ_OOP_AGE_G.average <- c(results.ADJ_OOP_AGE_G.average,
                                       wtd.mean(data.temp$ADJ_OOP, data.temp$WGT))
  }
}
rm(i, j, data.temp)
gc()
results.ADJ_OOP_AGE_G <- data.frame(
  year = as.character(results.ADJ_OOP_AGE_G.year),
  age = factor(results.ADJ_OOP_AGE_G.age),
  average = results.ADJ_OOP_AGE_G.average
)
rm(
  results.ADJ_OOP_AGE_G.year,
  results.ADJ_OOP_AGE_G.age,
  results.ADJ_OOP_AGE_G.average
)
gc()
par(mar = c(0, 0, 0, 0))
ggplot(data = results.ADJ_OOP_AGE_G, aes(
  x = age,
  y = average,
  group = year,
  color = year
)) +
  geom_line(size = 1, aes(linetype = year)) +
  geom_point(size = 3, aes(shape = year)) +
  scale_x_discrete(breaks = 1:age.group.num, labels = age.group) +
  theme(
    legend.position = "top",
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
  height = figure.height + 0.5
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
  quatiles.temp <- wtd.quantile(data.temp$ADJ_OOP,
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
  quatiles.temp <- wtd.quantile(data.temp$ADJ_OOP,
                                weights = data.temp$WGT,
                                probs = c(.25, .5, .75))
  results.ADJ_OOP_IQR_NOINS.average <- c(results.ADJ_OOP_IQR_NOINS.average, quatiles.temp)
  quatiles.temp.NOINS <- wtd.quantile(
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
  geom_line(size = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
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
  geom_line(size = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
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
  geom_line(size = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
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
  geom_line(size = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
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

# Adjusted OOP by Quantiles (Unit-Free) ----------------------------------------
results.ADJ_OOP_IQR_DIFF_ADJ.year <- c()
results.ADJ_OOP_IQR_DIFF_ADJ.status <- c()
results.ADJ_OOP_IQR_DIFF_ADJ.average <- c()
for (year in years) {
  results.ADJ_OOP_IQR_DIFF_ADJ.year <- c(results.ADJ_OOP_IQR_DIFF_ADJ.year, rep(year, 2))
  results.ADJ_OOP_IQR_DIFF_ADJ.status <- c(results.ADJ_OOP_IQR_DIFF_ADJ.status, c("IQCD", "MAD(%)"))
  data.temp <- MEPS_all[MEPS_all$YEAR == year, ]
  quatiles.temp <- wtd.quantile(data.temp$ADJ_OOP,
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
  geom_line(size = 1, aes(linetype = status)) +
  geom_point(size = 3, aes(shape = status)) +
  scale_x_discrete(breaks = year.break) +
  theme(
    legend.position = "top",
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