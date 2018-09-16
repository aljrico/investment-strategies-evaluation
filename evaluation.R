library(tidyverse)
library(data.table)
library(feather)
library(tictoc)
source("functions.R")

worth <- 0
trials <- 1
n_stocks <- 500
alpha_result <- list()
tic()
for(t in 1:trials){
	stocks_universe <- getTickers("sp500") %>% sample(n_stocks) %>% intersect(list.files("data/"))
	files <- paste0("data/",stocks_universe)
	dt <- rbindlist(lapply(files, read_feather)) %>% filter(price >= 0.1) %>% data.table()

	investment <- simulate_investment(data = dt, amount_to_invest = 1, strategy = "per", specificity = 2)

	alpha_result[[t]] <- alpha(portfolio = investment$result$per, benchmark = investment$result$benchmark)
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

names(alpha_result) <- paste0("V",as.character(seq(1:trials)))
alpha_result %>%
	as_tibble() %>%
	.$V1 %>%
	ts.plot()
