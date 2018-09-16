library(tidyverse)
library(data.table)
library(progress)

# Formulas ----------------------------------------------------------------

beta <- function(portfolio, benchmark){
	return(cov(portfolio,benchmark/sd(benchmark)^2))
}

# compute_return <- function(serie, invested_capital, months){
# 	final_value <- serie %>% tail(1)
# 	ret <- (1/months)*(-1 + (1 + (8*(final_value))/(invested_capital*months))^(1/2))*100
# 	return(ret)
# }

alpha <- function(portfolio, benchmark){
	a <- tail(portfolio,1) - beta(portfolio, benchmark)*tail(benchmark,1)
	return(a)
}

# Investing Criteria ------------------------------------------------------

invest <- function(method = "random", data, date, amount_to_invest = 1){
	stocks_available <- tidy_merged %>% filter(date == d) %>% .$firm %>% unique() %>% as.character()

	if(method == "random"){
		selected_stock <- stocks_available %>% sample(1)
	}

	if(method == "per"){
		selected_stock <- tidy_merged %>%
			filter(date == d) %>%
			filter(firm %in% stocks_available) %>%
			mutate(per = price/TotalRevenue) %>%
			top_n(1, -per) %>%
			.$firm %>%
			unique() %>%
			as.character()
	}

	if(is.null(stocks_owned[[selected_stock]])) {
		stocks_owned[[selected_stock]] <- 0
	}
	stocks_owned[[selected_stock]] <- stocks_owned[[selected_stock]] + amount_to_invest

	return(stocks_owned)
}

# Simulation --------------------------------------------------------------

dates <- unique(tidy_merged$date)
portfolio <- tibble()
stocks_owned <- list()
stocks_owned_benchmark <- list()
amount_to_invest <- 1
pb <- progress_bar$new(total = length(dates))
for(i in 1:length(dates)){
	pb$tick()

	d <- dates[[i]]

	# Invest first day of each month
	if(!(class(try(dates[[i-1]], silent = TRUE)) == 'try-error')){
		if(day(dates[[i]]) < day(dates[[i-1]])){
			stocks_owned_benchmark <- invest(method = "random", data = tidy_merged, date = d, amount_to_invest = amount_to_invest)
			stocks_owned <- invest(method = "per", data = tidy_merged, date = d, amount_to_invest = amount_to_invest)
		}
	}else{
		stocks_owned_benchmark <- invest(method = "random", data = tidy_merged, date = d, amount_to_invest = amount_to_invest)
		stocks_owned <- invest(method = "per", data = tidy_merged, date = d, amount_to_invest = amount_to_invest)
	}

	# Compute Portfolio value
	today_portfolio <- tibble(date = d)
	today_prices <- tidy_merged %>% filter(date == d)

	tmp_value <- 0
	tmp_benchmark <- 0
	for(n in names(stocks_owned)){
		tmp_value <- tmp_value + sum(stocks_owned[[n]]*(today_prices %>% filter(firm == n) %>% .$price))
		tmp_benchmark <- tmp_benchmark + sum(stocks_owned_benchmark[[n]]*(today_prices %>% filter(firm == n) %>% .$price))
	}

	today_portfolio$value <- tmp_value
	today_portfolio$benchmark <- tmp_benchmark
	portfolio <- rbind(portfolio,today_portfolio)

}



if(alpha(portfolio = portfolio$value, benchmark = portfolio$benchmark)>0){
	worth <- worth + 1
	}

portfolio %>%
	melt(id.vars = "date") %>%
	ggplot() +
	geom_line(aes(x = date, y = value, colour = variable))

