library(tidyverse)
library(data.table)
library(feather)
library(tictoc)
library(harrypotter)
source("functions.R")

trials <- 50
n_stocks <- 100
specificity <- 25
alpha_result <- list()
diff_result <- list()

strategies_list <- c("random", "per", "evebitda", "dividends_ev", "div_and_rev")

tic()
for(t in 1:trials){

	stocks_universe <- getTickers("all") %>% sample(n_stocks) %>% intersect(list.files("data/"))
	files <- paste0("data/",stocks_universe)
	dt <- rbindlist(lapply(files, read_feather)) %>% filter(price >= 0.1) %>% data.table()
	investment <- simulate_multiple_strategies_fasto(data = dt, amount_to_invest = 1, strategies = strategies_list, specificity = specificity)

	for(s in strategies_list) {
		alpha_result[[s]][[t]] <- alpha(portfolio = investment[["results"]] %>% filter(strategy == s) %>% .$value,
																		benchmark = investment[["results"]] %>% filter(strategy == "random") %>% .$value
																		)
		diff_result[[s]][[t]] <- alpha_ret(portfolio = investment[["results"]] %>% filter(strategy == s) %>% .$value,
																	 benchmark = investment[["results"]] %>% filter(strategy == "random") %>% .$value
		)
	}
}
toc()

investment$result %>%
	ggplot(aes(x = date, y = value, colour = strategy)) +
	geom_line(size = 1) +
	theme_minimal() +
	scale_colour_hp(discrete = TRUE, house = "ravenclaw") +
	ylab("Value")

alpha_result %>%
	as_tibble() %>%
	melt() %>%
	ggplot(aes(x = as.factor(variable), y = value)) +
	geom_boxplot(aes(fill = variable), colour = "black", alpha = 0.35) +
	geom_point(aes(colour = variable), size = 2) +
	theme_minimal() +
	xlab("Strategy") +
	ylab("Alpha")+
	scale_colour_hp(discrete = TRUE, house = "ravenclaw", direction = -1, name = "Strategy") +
	scale_fill_hp(discrete = TRUE, house = "ravenclaw", direction = -1, name = "Strategy")

diff_result %>%
	as_tibble() %>%
	melt() %>%
	ggplot(aes(x = as.factor(variable), y = value)) +
	geom_boxplot(aes(fill = variable), colour = "black", alpha = 0.35) +
	geom_point(aes(colour = variable), size = 2) +
	theme_minimal() +
	xlab("Strategy") +
	ylab("Returns Relative Difference")+
	scale_colour_hp(discrete = TRUE, house = "ravenclaw", direction = -1, name = "Strategy") +
	scale_fill_hp(discrete = TRUE, house = "ravenclaw", direction = -1, name = "Strategy")

investment$portfolio$div_and_rev %>%
	unlist() %>%
	t() %>% t() %>%
	as.data.frame() %>%
	rownames_to_column("company") %>%
	mutate(weight = V1/sum(V1)) %>%
	top_n(10, weight) %>%
	ggplot(aes(x = reorder(company, -weight), y = weight, fill = weight)) +
	geom_col(colour = "black") +
	theme_minimal() +
	scale_fill_hp(house = "slytherin", name = "Weight") +
	ylab("Weight") +
	xlab("") +
	theme(legend.position = "none")
