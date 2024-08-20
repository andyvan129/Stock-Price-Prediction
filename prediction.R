# building a stock price prediction model using historic SPY data

# install required packages
packages <- c('dplyr', 'caret', 'tidyquant', 'ggplot2', 'zoo')
for (pack in packages) {
  if (!require(pack, character.only = TRUE)) install.packages(pack)
  library(pack, character.only = TRUE)
}

rm(pack, packages)

# loading stock pricing data
stock_raw <- getSymbols(Symbols = 'SPY', auto.assign = FALSE) %>%
  `colnames<-`(c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted_Close'))

# Compute price changes and create prediction target (y)
stock <- stock_raw %>%
  data.frame() %>%
  mutate(change = Close - Open, change_pct = change * 100 / Open) %>%
  mutate(direction = ifelse(change_pct > 0, 1, 0), y = lead(direction)) %>%
  na.omit() %>%
  mutate(direction = NULL) # remove the known "direction" column.

# Compute volume changes compared to average volume of the previous n days
stock <- stock %>%
  mutate(avg_vol_10 = rollmean(stock$Volume, 10, fill = NA, align = 'right')) %>%
  na.omit() %>%
  mutate(vol_10 = (Volume - avg_vol_10) / Volume)

# Additional predictors can be added at this step

# Filter out predictor columns
# This also converts a timeseries data to individually unrelated data
metrics <- c('change_pct', 'Volume', 'y')
metrics <- stock %>%
  select(any_of(metrics))

# split into train and test datasets
test_ind <- createDataPartition(metrics$y, times = 1, p = 0.1, list = FALSE)
test <- metrics[test_ind, ]
train <- metrics[-test_ind, ]

# setup training control
control <- trainControl(method = 'cv', number = 10, p = 0.9)