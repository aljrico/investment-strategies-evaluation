library(tidyverse)
library(data.table)
library(feather)
library(tictoc)
source("functions.R")

trials <- 1
n_stocks <- 100
specificity <- 2
alpha_result <- c()
tic()
for(t in 1:trials){
	stocks_universe <- getTickers("all") %>% sample(n_stocks) %>% intersect(list.files("data/"))
	files <- paste0("data/",stocks_universe)
	dt <- rbindlist(lapply(files, read_feather)) %>% filter(price >= 0.1) %>% data.table()

	investment <- simulate_investment(data = dt, amount_to_invest = 1, strategy = "dividends_ev", specificity = specificity)

	alpha_result[[t]] <- alpha(portfolio = investment$result$evebitda, benchmark = investment$result$benchmark) %>% tail(150) %>%  sum()
}
toc()

investment$result %>%
	melt(id.vars = c("date")) %>%
	ggplot(aes(x = date, y = value, colour = variable)) +
	geom_line(size = 1) +
	theme_minimal()

investment$portfolio_strategy %>%
	unlist() %>%
	t() %>% t() %>%
	as.data.frame() %>%
	rownames_to_column("company") %>%
	mutate(weight = V1/sum(V1)) %>%
	top_n(10, weight) %>%
	ggplot(aes(x = reorder(company, -weight), y = weight)) +
	geom_col(colour = "black") +
	theme_minimal()

investment$portfolio_benchmark %>%
	unlist() %>%
	t() %>% t() %>%
	as.data.frame() %>%
	rownames_to_column("company") %>%
	mutate(weight = V1/sum(V1)) %>%
	top_n(10, weight) %>%
	ggplot(aes(x = reorder(company, -weight), y = weight)) +
	geom_col(colour = "black") +
	theme_minimal()

alpha_result %>%
	as_tibble() %>%
	ggplot(aes(x = value)) +
	geom_histogram(colour = "black") +
	theme_minimal()
