# building a stock price prediction model using historic SPY data

# install required packages
packages <- c('dplyr', 'caret', 'tidyquant', 'ggplot2', 'zoo', 'randomForest', 'roll', 'parallel', 'doParallel')
for (pack in packages) {
  if (!require(pack, character.only = TRUE)) install.packages(pack)
  library(pack, character.only = TRUE)
}

# setup parallel computation
num_core <- detectCores() - 1
pl <- makeCluster(num_core)
registerDoParallel(pl)

rm(pack, packages, num_core)

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


# Compute rolling price averages and recent high/lows
moving_avg_n <- c(10)
moving_avg <- list()
for (n in moving_avg_n){
  
  # Rolling average price changes
  col_name <- paste('sma', n, sep = '_')
  moving_avg[[col_name]] <- (stock$Close - SMA(stock$Close, n = n)) * 100 / stock$Close
  col_name <- paste('ema', n, sep = '_')
  moving_avg[[col_name]] <- (stock$Close - EMA(stock$Close, n = n)) * 100 / stock$Close
  col_name <- paste('evwma', n, sep = '_')
  moving_avg[[col_name]] <- (stock$Close - EVWMA(stock$Close, stock$Volume, n = n)) * 100 / stock$Close
  
  # Rolling min/max
  col_name <- paste('high', n, sep = '_')
  moving_avg[[col_name]] <- (stock$Close - rollmax(stock$High, n, fill = NA, align = 'right')) / stock$Close
  col_name <- paste('low', n, sep = '_')
  moving_avg[[col_name]] <- (stock$Close - -rollmax(-stock$Low, n, fill = NA, align = 'right')) / stock$Close
  
  # Rolling volume
  col_name <- paste('vol', n, sep = '_')
  moving_avg[[col_name]] <- (stock$Volume - rollmean(stock$Volume, n, fill = NA, align = 'right')) / stock$Volume
  
  # Rolling standard deviation (volatility)
  col_name <- paste('sd', n, sep = '_')
  moving_avg[[col_name]] <- roll_sd(stock$change_pct, n)
}
stock <- stock %>%
  cbind(moving_avg)
rm(moving_avg, moving_avg_n, col_name, n)


# Additional predictors can be added at this step


# Filter out predictor columns
# This also converts a timeseries data to individually unrelated data
metrics <- stock %>%
  select(-c('Open', 'Close', 'High', 'Low', 'Volume', 'Adjusted_Close')) %>%
  na.omit()


# split into train and test datasets
test_ind <- createDataPartition(metrics$y, times = 1, p = 0.1, list = FALSE)
final_test <- metrics[test_ind, ]
train <- metrics[-test_ind, ]

# carve a portion out of training set for model tuning and ensemble purpose
train_test_ind <- createDataPartition(train$y, times = 1, p = 0.1, list = FALSE)
train_test <- train[train_test_ind, ]
train_train <- train[-train_test_ind, ]

rm(test_ind, train_test_ind, train)


# setup training control
control <- trainControl(method = 'cv', number = 10, p = 0.9)

# train some models
fit <- list()
fit[['knn']] <- train(y ~ ., data = train_train, trControl = control, method = 'knn', tuneGrid = data.frame(k = seq(1, 15, 2)))
fit[['rf']] <- train(y ~ ., data = train_train, trControl = control, method = 'rf', tuneGrid = data.frame(mtry = seq(2, 12, 1)))
fit[['glm']] <- train(y ~ ., data = train_train, trControl = control, method = 'glm')
fit[['xgbTree']] <- train(y ~ ., data = train_train, trControl = control, method = 'xgbTree')
fit[['rpart']] <- train(y ~ ., data = train_train, trControl = control, method = 'rpart', tuneGrid = data.frame(cp = seq(0.005, 0.03, 0.005)))


# test and select models
ensemble_models <- 3
test_data <- final_test

pred <- list()
for (f in fit){
  pred[[f$method]] <- predict(f, test_data)
}
ensemble <- pred %>%
  data.frame() %>%
  mutate(count = rowSums(. == 1)) %>%
  mutate(y_hat = ifelse(count >= ensemble_models, 1, 0),
         y_hat = factor(y_hat))


confusionMatrix(ensemble$y_hat, test_data$y)

test_data %>%
  mutate(y_hat = ensemble$y_hat,
         y_hat = as.numeric(y_hat)) %>%
  summarise(sum(change * y_hat), sum(change))