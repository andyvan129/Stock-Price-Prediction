# building a stock price prediction model using historic SPY data

# install required packages
packages <- c('dplyr', 'caret', 'tidyquant', 'ggplot2')
for (pack in packages) {
  if (!require(pack, character.only = TRUE)) install.packages(pack)
  library(pack, character.only = TRUE)
}

rm(pack, packages)

# loading stock pricing data
stock <- getSymbols(Symbols = 'SPY', auto.assign = FALSE) %>%
  `colnames<-`(c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted_Close'))

# visualize price changes
chart_Series(stock)

# Compute price changes and create prediction target (y)
stock <- stock %>%
  data.frame() %>%
  mutate(change = Close - Open, change_pct = change * 100 / Open) %>%
  mutate(direction = ifelse(change_pct > 0, 1, 0), y = lead(direction)) %>%
  na.omit() %>%
  mutate(direction = NULL) # remove the known "direction" column.

# Additional predictors can be added at this step

# Filter out predictor columns
# This also converts a timeseries data to individually unrelated data
predictor_list <- c('change_pct', 'Volume')
x <- stock %>%
  select(any_of(predictor_list))