library(tidyverse)
library(data.table)
library(quantmod)
library(rvest)
library(progress)
library(lubridate)
library(feather)
library(tictoc)

# Custom Functions --------------------------------------------------------

source("get_financials.R")
source("get_tickers.R")
source("get_prices.R")
source("get_merge.R")


# Retrieve Data -----------------------------------------------------------

tickers <- getTickers(index = "all")

pb <- progress_bar$new(total = length(tickers))
tic()
for(t in tickers){
	pb$tick()
	tryCatch({
		fins <- getFinancials(t)
		prices <- getPrices(t)
		whole_data <- getMerge(financials = fins, prices = prices)
		path <- paste0("data/",t)
		write_feather(whole_data,path)
	}, error=function(e){})
}
toc()
