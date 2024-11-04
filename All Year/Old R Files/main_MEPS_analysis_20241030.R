# Load necessary libraries
rm(list = ls())
#library(foreign)
#library(devtools)
#library(tidyverse)
#library(readr)
#library(readxl)
#library(haven)
#library(survey)
library(Hmisc)
#library(spatstat.geom)
library(ggplot2)
theme_set(theme_bw())
library(ggpattern)
library(viridis)

# Data directory
dic_data <- "C:/Users/user/Desktop/Medical Uncertainty 1996-2020/All Year/Raw Data/"
dic_fig <- "C:/Users/user/Desktop/Medical Uncertainty 1996-2020/All Year/Figure/"

# Wrapper function to process a single year's MEPS data
process_year_data <- function(year, dic) {
  
  # Load data and abstract the last two digits of the data year
  dt <- readRDS(paste0(dic, 'MEPS_', year, '.RData'))

  # Compute quantile
  mean <- wtd.mean(dt$ADJOOP, dt$WGTNEW, na.rm = TRUE)
  variance <- wtd.var(dt$ADJOOP, dt$WGTNEW, na.rm = TRUE)
  quantile <- wtd.quantile(dt$ADJOOP, dt$WGTNEW,
                           probs = c(0.10, 0.25, 0.50, 0.75, 0.90, 0.95), 
                           na.rm = TRUE)
  mean_NOINS <- wtd.mean(dt$ADJOOP * dt$NOINS, dt$WGTNEW * dt$NOINS, na.rm = TRUE)
  
  # Construct a structured dataframe with results for the year
  result <- data.frame(
    Year = year,
    Number = dim(dt)[1],
    Number_NOINS = sum(dt$NOINS),
    Share_NOINS = sum(dt$NOINS * dt$WGTNEW) / sum(dt$WGTNEW),
    OOP_Mean = mean,
    OOP_Variance = variance,
    OOP_Quantile_10 = quantile[1],
    OOP_Quantile_25 = quantile[2],
    OOP_Median = quantile[3],
    OOP_Quantile_75 = quantile[4],
    OOP_Quantile_90 = quantile[5],
    OOP_Quantile_95 = quantile[6],
    OOP_Mean_NOINS = mean_NOINS
  )
  
  return(result)
}

# List of years to process
years <- c(1996:2020)
results <- lapply(years, function(year) process_year_data(as.character(year), dic_data))

# Combine results
combined_results <- do.call(rbind, results)
row.names(combined_results) <- NULL
combined_results

# Figures
figure.width <- 6
figure.height <- 3.5

# Share of not insured
par(mar = c(0, 0, 0, 0))
ggplot(data = combined_results, aes(x = Year, y = Number, group = 1)) + 
  geom_line(color = "blue", size = 1) + 
  geom_point(color = "blue", size = 3) + 
  theme(legend.position="top", text = element_text(size=18, family="serif"), 
        legend.title=element_text(size=15), legend.text=element_text(size=15),
        axis.text.x = element_text(size=13), axis.text.y = element_text(size=13)) + 
  labs(x = "year", y = "") +
  scale_x_discrete(breaks = seq(years[1], years[length(years)], by = 4))
dev.copy(pdf, paste0(dic_fig,"size.pdf"), width=figure.width, height=figure.height)
dev.off()

# Share of not insured
par(mar = c(0, 0, 0, 0))
ggplot(data = combined_results, aes(x = Year, y = Share_NOINS * 100, group = 1)) + 
  geom_line(color = "blue", size = 1) + 
  geom_point(color = "blue", size = 3) + 
  theme(legend.position="top", text = element_text(size=18, family="serif"), 
        legend.title=element_text(size=15), legend.text=element_text(size=15),
        axis.text.x = element_text(size=13), axis.text.y = element_text(size=13)) + 
  labs(x = "year", y = "%") +
  scale_x_discrete(breaks = seq(years[1], years[length(years)], by = 4))
dev.copy(pdf, paste0(dic_fig,"share_of_uninsured.pdf"), width=figure.width, height=figure.height)
dev.off()

# Mean of OOP
par(mar = c(0, 0, 0, 0))
ggplot(data = combined_results, aes(x = Year, y = OOP_Mean, group = 1)) + 
  geom_line(color = "blue", size = 1) + 
  geom_point(color = "blue", size = 3) + 
  theme(legend.position="top", text = element_text(size=18, family="serif"), 
        legend.title=element_text(size=15), legend.text=element_text(size=15),
        axis.text.x = element_text(size=13), axis.text.y = element_text(size=13)) + 
  labs(x = "year", y = "USD") +
  scale_x_discrete(breaks = seq(years[1], years[length(years)], by = 4))
dev.copy(pdf, paste0(dic_fig,"OOP_mean.pdf"), width=figure.width, height=figure.height)
dev.off()

par(mar = c(0, 0, 0, 0))
ggplot()+
  geom_line(data = combined_results, aes(x = Year, y = OOP_Mean, group = 1), color = "blue", size = 1) + 
  geom_point(data = combined_results, aes(x = Year, y = OOP_Mean, group = 1), color = "blue", size = 3) + 
  geom_line(data = combined_results, aes(x = Year, y = OOP_Mean_NOINS, group = 2), color = "red", size = 1) + 
  geom_point(data = combined_results, aes(x = Year, y = OOP_Mean_NOINS, group = 2), color = "red", size = 3) + 
  theme(legend.position="top", text = element_text(size=18, family="serif"), 
        legend.title=element_text(size=15), legend.text=element_text(size=15),
        axis.text.x = element_text(size=13), axis.text.y = element_text(size=13)) + 
  labs(x = "year", y = "USD") +
  scale_x_discrete(breaks = seq(years[1], years[length(years)], by = 4))
dev.copy(pdf, paste0(dic_fig,"OOP_mean.pdf"), width=figure.width, height=figure.height)
dev.off()