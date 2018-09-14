library(tidyverse)
library(data.table)
library(quantmod)
library(rvest)
library(progress)
library(lubridate)

source("get_financials.R")
source("get_tickers.R")


# Retrieve Data -----------------------------------------------------------

tickers <- getTickers(index = "sp400")
tickers <- c("AAPL", "GOOGL", "AMZN", "FB")
fins <- getFinancials(tickers)


# Tidy Data ---------------------------------------------------------------

# Prices
stock_prices <- new.env()
getSymbols(tickers, env = stock_prices, src = "yahoo", from = "1950-01-01", auto.assign = TRUE)

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


# Financials
fins[is.na(fins)] <- 0



# Plots -------------------------------------------------------------------

fins %>%
	ggplot(aes(colour = firm)) +
	geom_point(aes(x = date, y = as.numeric(TotalRevenue)/as.numeric(LongTermDebt))) +
	theme_bw()

fins %>%
	ggplot(aes(colour = firm)) +
	geom_point(aes(x = date, y = as.numeric(LongTermInvestments)/as.numeric(TotalRevenue))) +
	geom_line(aes(x = date, y = as.numeric(LongTermInvestments)/as.numeric(TotalRevenue))) +
	theme_minimal()


# Filtering ---------------------------------------------------------------

fins %>%
	mutate(invest_rev_ratio = as.numeric(LongTermInvestments)/as.numeric(TotalRevenue)) %>%
	top_n(20, as.numeric(IncomeBeforeTax)) %>%
	ggplot(aes(colour = firm)) +
	geom_point(aes(x = date, y = as.numeric(LongTermInvestments)/as.numeric(TotalRevenue))) +
	geom_line(aes(x = date, y = as.numeric(LongTermInvestments)/as.numeric(TotalRevenue))) +
	theme_minimal()



