# building a stock price prediction model using historic SPY data

# install required packages
packages <- c('dplyr', 'caret')
for (pack in packages) {
  if (!require(pack, character.only = TRUE)) install.packages(pack)
  library(pack, character.only = TRUE)
}

# setting up git