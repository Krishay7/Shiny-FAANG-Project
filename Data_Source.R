# This script explains how the stock price data for the FAANG companies (Meta, Amazon, Apple, Netflix, Google) 
# is retrieved using the tq_get() function within the tidyquant package.

# FAANG stock symbols
FAANG <- c("META", "AMZN", "AAPL", "NFLX", "GOOG")

# Fetch stock price data for the past 5 years from Yahoo Finance
# Using the tidyquant package to fetch adjusted stock prices
faang_prices <- tq_get(FAANG, get = "stock.prices", from = Sys.Date() - years(5), complete_cases = TRUE)

# The 'faang_prices' data frame now contains the stock prices for the FAANG companies for the past 5 years.
# This data is used within the Shiny app for analysis and visualization.