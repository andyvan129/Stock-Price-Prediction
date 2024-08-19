# building a stock price prediction model using historic SPY data

# install required packages
packages <- c('dplyr', 'caret', 'tidyquant', 'ggplot2')
for (pack in packages) {
  if (!require(pack, character.only = TRUE)) install.packages(pack)
  library(pack, character.only = TRUE)
}

rm(pack, packages)

# loading stock pricing data
SPY <- data.frame(getSymbols(Symbols = 'SPY', auto.assign = FALSE)) %>%
  `colnames<-`(c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted_Close'))

chart_Series(SPY)

SPY[order(desc(as_datetime(rownames(SPY)))), ] %>%
  top_n(10)