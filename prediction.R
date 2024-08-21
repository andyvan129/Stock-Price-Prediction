# building a stock price prediction model using historic SPY data

# install required packages
packages <- c('dplyr', 'caret', 'tidyquant', 'ggplot2', 'zoo', 'randomForest')
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
  mutate(change = Close - Open, change_pct = change * 100 / Open,
         high_pct = (High - Open) * 100 / Open,
         low_pct = (Low - Open) * 100 / Open) %>%
  mutate(direction = ifelse(change_pct > 0, 1, 0), y = lead(direction),
         y = factor(y)) %>%
  mutate(direction = NULL) # remove the known "direction" column.


# Compute volume changes compared to average volume of the previous n days
avg_vol_n <- c(10, 50)
avg_vol <- list()
for (n in avg_vol_n){
  col_name <- paste('vol', n, sep = '_')
  avg_vol[[col_name]] <- rollmean(stock$Volume, n, fill = NA, align = 'right')
  avg_vol[[col_name]] <- (stock$Volume - avg_vol[[col_name]]) / stock$Volume
}
stock <- stock %>%
  cbind(avg_vol)


# Compute rolling price averages and recent high/lows



# Plot a chart checking if there is any correlation
stock %>%
  ggplot(aes(x = change_pct, y = vol_50, col = y)) +
  geom_point() +
  scale_y_sqrt() +
  scale_x_sqrt()
# clearly not....


# Additional predictors can be added at this step

# Filter out predictor columns
# This also converts a timeseries data to individually unrelated data
metrics <- c('change_pct', 'high_pct', 'low_pct', 'vol_10', 'vol_50', 'y')
metrics <- stock %>%
  select(any_of(metrics)) %>%
  na.omit()

# split into train and test datasets
test_ind <- createDataPartition(metrics$y, times = 1, p = 0.1, list = FALSE)
final_test <- metrics[test_ind, ]
train <- metrics[-test_ind, ]

train_test_ind <- createDataPartition(train$y, times = 1, p = 0.1, list = FALSE)
train_test <- train[train_test_ind, ]
train_train <- train[-train_test_ind, ]

# setup training control
control <- trainControl(method = 'cv', number = 10, p = 0.9)

# train some models
models <- c('knn', 'rf')
fit <- list()
for (model in models){
  fit[[model]] <- train(y ~ ., data = train_train, trControl = control, method = model)
}


# test and select models
pred <- list()
for (f in fit){
  pred[[f$method]] <- predict(f, test)
}
data.frame(pred)
