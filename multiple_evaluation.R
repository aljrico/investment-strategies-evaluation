library(tidyverse)
library(data.table)
library(feather)
library(tictoc)
source("functions.R")

trials <- 30
n_stocks <- 20
specificity <- 1
alpha_result <- list()

strategies_list <- c("random", "per", "evebitda", "dividends_ev")

tic()
for(t in 1:trials){
	cat(paste0("... ", t, "... \n"))
	stocks_universe <- getTickers("all") %>% sample(n_stocks) %>% intersect(list.files("data/"))
	files <- paste0("data/",stocks_universe)
	dt <- rbindlist(lapply(files, read_feather)) %>% filter(price >= 0.1) %>% data.table()
	investment <- simulate_multiple_strategies(data = dt, amount_to_invest = 1, strategies = strategies_list, specificity = specificity)

	for(s in strategies_list) alpha_result[[s]][[t]] <- alpha(portfolio = investment[["results"]][[s]], benchmark = investment[["results"]][["random"]]) %>% top_n(150,date) %>%  sum()
}
toc()

investment$result %>%
	melt(id.vars = c("date")) %>%
	ggplot(aes(x = date, y = value, colour = variable)) +
	geom_line(size = 1) +
	theme_minimal()

alpha_result %>%
	as_tibble() %>%
	melt() %>%
	ggplot(aes(x = as.factor(variable), y = value)) +
	geom_boxplot(aes(fill = variable), colour = "black", alpha = 0.35) +
	geom_point(aes(colour = variable), size = 2) +
	theme_minimal() +
	xlab("Strategy") +
	ylab("Alpha")
