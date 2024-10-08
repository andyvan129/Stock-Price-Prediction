# *ATTEMPTING* to build a stock price prediction model using historic SPY data

# install required packages
packages <- c('dplyr', 'caret', 'tidyquant', 'zoo', 'roll', 'parallel', 'doParallel')
for (pack in packages) {
  if (!require(pack, character.only = TRUE)) install.packages(pack)
  library(pack, character.only = TRUE)
}

# setup parallel computation for multi-core computers
if (detectCores() > 1){
  num_core <- detectCores() - 1
  pl <- makeCluster(num_core)
  registerDoParallel(pl)
} else {
  registerDoSEQ()
}


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


# split data before creating predictors
train <- stock[1:3000, ]
ensemble_test <- stock[3001:4000, ]
final_test <- stock[4001:nrow(stock), ]


# Compute rolling price averages and recent high/lows
moving_avg_n <- c(10, 20, 40, 80)

rolling <- function(x){
  data <- x
  moving_avg <- list()
  for (n in moving_avg_n){
    # Rolling average price changes
    col_name <- paste('sma', n, sep = '_')
    moving_avg[[col_name]] <- (data$Close - SMA(data$Close, n = n)) * 100 / data$Close
    col_name <- paste('ema', n, sep = '_')
    moving_avg[[col_name]] <- (data$Close - EMA(data$Close, n = n)) * 100 / data$Close
    col_name <- paste('evwma', n, sep = '_')
    moving_avg[[col_name]] <- (data$Close - EVWMA(data$Close, data$Volume, n = n)) * 100 / data$Close
    
    # Rolling min/max
    col_name <- paste('high', n, sep = '_')
    moving_avg[[col_name]] <- (data$Close - rollmax(data$High, n, fill = NA, align = 'right')) / data$Close
    col_name <- paste('low', n, sep = '_')
    moving_avg[[col_name]] <- (data$Close - -rollmax(-data$Low, n, fill = NA, align = 'right')) / data$Close
    
    # Rolling volume
    col_name <- paste('vol', n, sep = '_')
    moving_avg[[col_name]] <- (data$Volume - rollmean(data$Volume, n, fill = NA, align = 'right')) / data$Volume
    
    # Rolling standard deviation (volatility)
    col_name <- paste('sd', n, sep = '_')
    moving_avg[[col_name]] <- roll_sd(data$change_pct, n)
  }
  x <- x %>%
    cbind(moving_avg)
  return(x)
}

train <- rolling(train)
final_test <- rolling(final_test)
ensemble_test <- rolling(ensemble_test)


# Filter out predictor columns
filt_columns <- function(x){
  x <- x %>%
    select(-c('Open', 'Close', 'High', 'Low', 'Volume', 'Adjusted_Close')) %>%
    na.omit()
  return(x)
}

train <- filt_columns(train)
final_test <- filt_columns(final_test)
ensemble_test <- filt_columns(ensemble_test)

# setup training control
control <- trainControl(method = 'timeslice', initialWindow = 500, horizon = 300, fixedWindow = FALSE, skip = 49)

# train some models
fit <- list()
fit[['knn']] <- train(y ~ ., data = train, trControl = control, method = 'knn', tuneGrid = data.frame(k = seq(1, 15, 2)))
fit[['rf']] <- train(y ~ ., data = train, trControl = control, method = 'rf', tuneGrid = data.frame(mtry = seq(2, ncol(train), 2)))
fit[['glm']] <- train(y ~ ., data = train, trControl = control, method = 'glm')
fit[['xgbTree']] <- train(y ~ ., data = train, trControl = control, method = 'xgbTree')
fit[['rpart']] <- train(y ~ ., data = train, trControl = control, method = 'rpart', tuneGrid = data.frame(cp = seq(0.005, 0.05, 0.005)))
fit[['earth']] <- train(y ~ ., data = train, trControl = control, method = 'earth')


# ensemble models and final testing
result <- function(x, test_data){
  pred <- list()
  for (f in fit){
    pred[[f$method]] <- predict(f, test_data)
  }
  
  ensemble <- pred %>%
    data.frame() %>%
    mutate(count = rowSums(. == 1)) %>%
    mutate(y_hat = ifelse(count >= x, 1, 0),
           y_hat = lag(y_hat),
           y_hat = factor(y_hat, levels = levels(test_data$y))) %>%
    na.omit()
  
  test_data <- test_data %>%
    mutate(y = lag(y)) %>%
    na.omit()
  
  ensemble <- cbind(test_data, ensemble) %>%
    select(y, y_hat, change)
  conf <- confusionMatrix(ensemble$y_hat, ensemble$y, positive = '1')
  ensemble <- ensemble %>% 
    mutate(y_hat = as.numeric(as.character(y_hat))) %>%
    summarise(portfolio = sum(ensemble$change * y_hat), 
              buy_and_hold = sum(ensemble$change),
              precision = conf$byClass['Precision'],
              sensitivity = conf$byClass['Sensitivity'],
              overall_accu = conf$overall['Accuracy'])
  
  return(ensemble)
}

ensemble_n <- seq(1, length(fit), 1)
lapply(ensemble_n, function(x) result(x, ensemble_test))
# choosing number of models based on highest precision

# Final testing
result(5, final_test)
