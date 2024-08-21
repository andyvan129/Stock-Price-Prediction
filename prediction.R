# building a stock price prediction model using historic SPY data

# install required packages
packages <- c('dplyr', 'caret', 'tidyquant', 'ggplot2', 'zoo', 'randomForest')
for (pack in packages) {
  if (!require(pack, character.only = TRUE)) install.packages(pack)
  library(pack, character.only = TRUE)
}

rm(pack, packages)


# loading stock pricing data
stock <- getSymbols(Symbols = 'SPY', auto.assign = FALSE) %>%
  `colnames<-`(c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted_Close'))


# Compute price changes and create prediction target (y)
stock <- stock %>%
  data.frame() %>%
  mutate(change = Close - Open, change_pct = change * 100 / Open,
         high_pct = (High - Open) * 100 / Open,
         low_pct = (Low - Open) * 100 / Open) %>%
  mutate(direction = ifelse(change_pct > 0, 1, 0), y = lead(direction),
         y = factor(y)) %>%
  na.omit() %>%
  mutate(direction = NULL) # remove the known "direction" column.


# Compute volume changes compared to average volume of the previous n days
avg_vol_n <- c(10, 25, 50, 100, 200)
lapply(avg_vol_n, function(x){
  col_name <- paste('vol', x, sep = '_')
  stock %>%
    mutate(col_name = rollmean(Volume, x, fill = NA, align = 'right'))
})
stock <- stock %>%
  mutate(avg_vol_10 = rollmean(stock$Volume, 10, fill = NA, align = 'right')) %>%
  na.omit() %>%
  mutate(vol_10 = (Volume - avg_vol_10) / Volume)


# Plot a chart checking if there is any correlation
stock %>%
  ggplot(aes(x = change_pct, y = vol_10, col = y)) +
  geom_point() +
  scale_y_sqrt() +
  scale_x_sqrt()
# clearly not....


# Additional predictors can be added at this step

# Filter out predictor columns
# This also converts a timeseries data to individually unrelated data
metrics <- c('change_pct', 'vol_10', 'y')
metrics <- stock %>%
  select(any_of(metrics))

# split into train and test datasets
test_ind <- createDataPartition(metrics$y, times = 1, p = 0.1, list = FALSE)
test <- metrics[test_ind, ]
train <- metrics[-test_ind, ]

# setup training control
control <- trainControl(method = 'cv', number = 10, p = 0.9)

# train some models
fit <- train(y ~ ., data = train, trControl = control, method = 'knn')
fit$results