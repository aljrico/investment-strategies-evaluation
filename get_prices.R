library(tidyverse)
library(data.table)
library(quantmod)
library(rvest)
library(progress)
library(lubridate)

getPrices <- function(tickers, from = "1950-01-01"){
	stock_prices <- new.env()
	getSymbols(tickers, src = "yahoo", from = "1950-01-01", auto.assign = TRUE, env = stock_prices)

	all_prices <- tibble()

	for(i in 1:length(tickers)){
		raw_df <- stock_prices[[tickers[[i]]]][,6] %>% data.frame()
		dates <- raw_df %>% row.names() %>% as.Date()
		prices <- raw_df[,1]

		new_df <- data.frame(price = as.numeric(prices), date = dates) %>%
			data.table()

		new_df$firm <- tickers[[i]]

		all_prices <- rbind(all_prices,new_df)
	}

	return(all_prices)
}
