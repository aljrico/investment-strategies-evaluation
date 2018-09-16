library(tidyverse)
library(data.table)
library(feather)
library(tictoc)
library(FinCal)
source("functions.R")

worth <- 0
trials <- 1
n_stocks <- 500
alpha_result <- c()
tic()
for(t in 1:trials){
	stocks_universe <- getTickers("sp500") %>% sample(n_stocks) %>% intersect(list.files("data/"))
	files <- paste0("data/",stocks_universe)
	dt <- rbindlist(lapply(files, read_feather)) %>% filter(price >= 0.1) %>% data.table()

	investment <- simulate_investment(data = dt, amount_to_invest = 1, strategy = "per")

	alpha_result[[t]] <- alpha(portfolio = investment$result$per, benchmark = investment$result$benchmark)
}
toc()

investment$result %>%
	melt(id.vars = c("date")) %>%
	ggplot(aes(x = date, y = value, colour = variable)) +
	geom_line(size = 1) +
	theme_minimal()

alpha_result %>%
	as_tibble() %>%
	ggplot(aes(x = value)) +
	geom_histogram(colour = "black") +
	theme_minimal()
