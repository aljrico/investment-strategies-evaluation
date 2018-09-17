
# compute_return <- function(serie, invested_capital, months){
# 	final_value <- serie %>% tail(1)
# 	ret <- (1/months)*(-1 + (1 + (8*(final_value))/(invested_capital*months))^(1/2))*100
# 	return(ret)
# }


beta <- function(portfolio, benchmark){
	return(cov(portfolio,benchmark/sd(benchmark)^2))
}

alpha <- function(portfolio, benchmark){
	if(tail(portfolio,1) <= 0){
		return(0)
	}else{
		a <- (portfolio - beta(portfolio, benchmark)*benchmark)/(portfolio + beta(portfolio, benchmark)*benchmark)
		return(a)
	}
}

invest <- function(method = "random", data, date, amount_to_invest = 1, stocks_owned, specificity = 1){
	d <- date
	stocks_available <- data %>% filter(date == d) %>% .$firm %>% unique() %>% as.character()

	if(method == "random"){
		selected_stock <- stocks_available %>% sample(specificity, replace = TRUE)
	}

	if(method == "per"){
		selected_stock <- data %>%
			filter(date == d) %>%
			filter(firm %in% stocks_available) %>%
			mutate(per = price/TotalRevenue) %>%
			top_n(specificity, -per) %>%
			.$firm %>%
			unique() %>%
			as.character()
	}

	if(method == "evebitda"){
		selected_stock <- data %>%
			filter(date == d) %>%
			filter(firm %in% stocks_available) %>%
			mutate(evebitda = (EarningsBeforeInterestandTaxes)/(price + PreferredStock + MinorityInterest + LongTermDebt - CashAndCashEquivalents)) %>%
			top_n(specificity, evebitda) %>%
			.$firm %>%
			unique() %>%
			as.character()
	}

	if(method == "dividends_ev"){
		selected_stock <- data %>%
			filter(date == d) %>%
			filter(firm %in% stocks_available) %>%
			mutate(dividends_ev = (DividendsPaid)/(price + PreferredStock + MinorityInterest + LongTermDebt - CashAndCashEquivalents)) %>%
			top_n(specificity, dividends_ev) %>%
			.$firm %>%
			unique() %>%
			as.character()
	}

	for(s in selected_stock){
		if(is.null(stocks_owned[[s]])) {
			stocks_owned[[s]] <- 0
		}
		stocks_owned[[s]] <- stocks_owned[[s]] + amount_to_invest/(specificity*(data %>% filter(date == d) %>% filter(firm == s) %>% .$price))
	}

	return(stocks_owned)
}

# Simulation --------------------------------------------------------------

simulate_investment <- function(data, amount_to_invest = 1, strategy = "per", specificity = 1){
	dates <- unique(data$date)
	portfolio <- tibble()
	stocks_owned <- list()
	stocks_owned_benchmark <- list()
	pb <- progress_bar$new(total = length(dates))
	for(i in 1:length(dates)){
		pb$tick()

		d <- dates[[i]]

		# Invest first day of each month
		if(!(class(try(dates[[i-1]], silent = TRUE)) == 'try-error')){
			if(day(dates[[i]]) < day(dates[[i-1]])){
				stocks_owned_benchmark <- invest(method = "random", data = data, date = d, amount_to_invest = amount_to_invest, stocks_owned = stocks_owned_benchmark, specificity = specificity)
				stocks_owned <- invest(method = strategy, data = data, date = d, amount_to_invest = amount_to_invest, stocks_owned = stocks_owned, specificity = specificity)
			}
		}else{
			stocks_owned_benchmark <- invest(method = "random", data = data, date = d, amount_to_invest = amount_to_invest, stocks_owned = stocks_owned_benchmark, specificity = specificity)
			stocks_owned <- invest(method = strategy, data = data, date = d, amount_to_invest = amount_to_invest, stocks_owned = stocks_owned, specificity = specificity)
		}

		# Compute Portfolio value
		today_portfolio <- tibble(date = d)
		today_prices <- data %>% filter(date == d)

		tmp_value <- 0
		tmp_benchmark <- 0
		for(n in names(stocks_owned)){
		lmao <-	data %>% filter(date == d) %>% .$firm %>% unique() %>% as.character()
		tmp_value <- tmp_value + sum(stocks_owned[[n]]*(today_prices %>% filter(firm == n) %>% .$price))
		}

		for(m in names(stocks_owned_benchmark)){
			tmp_benchmark <- tmp_benchmark + sum(stocks_owned_benchmark[[m]]*(today_prices %>% filter(firm == m) %>% .$price))
		}

		today_portfolio[[strategy]] <- tmp_value
		today_portfolio$benchmark <- tmp_benchmark
		portfolio <- rbind(portfolio,today_portfolio)
	}
	results <- portfolio
	portfolio_own <- stocks_owned
	portfolio_benchmark <- stocks_owned_benchmark
	return(list(results = results, portfolio_strategy = portfolio_own, portfolio_benchmark = portfolio_benchmark))
}


simulate_multiple_strategies <- function(data, amount_to_invest = 1, specificity = 1, strategies){
	dates <- unique(data$date) %>% sort()
	portfolio <- tibble()
	stocks_owned <- list()
	for(s in strategies) stocks_owned[[s]] <- list()

	for(i in 1:length(dates)){

		d <- dates[[i]]

		today_portfolio <- tibble(date = d)
		today_prices <- data %>% filter(date == d)

		for(strategy in (strategies)){

			# Invest first day of each month
			if(!(class(try(dates[[i-1]], silent = TRUE)) == 'try-error')){
				if(day(dates[[i]]) < day(dates[[i-1]])){
					stocks_owned[[strategy]] <- invest(method = strategy, data = data, date = d, amount_to_invest = amount_to_invest, stocks_owned = stocks_owned[[strategy]], specificity = specificity)
				}
			}else{
				stocks_owned[[strategy]] <- invest(method = strategy, data = data, date = d, amount_to_invest = amount_to_invest, stocks_owned = stocks_owned[[strategy]], specificity = specificity)
			}

			# Compute Portfolio value
			tmp_value <- 0
			tmp_benchmark <- 0
			for(n in names(stocks_owned[[strategy]])){
				lmao <-	data %>% filter(date == d) %>% .$firm %>% unique() %>% as.character()
				tmp_value <- tmp_value + sum(stocks_owned[[strategy]][[n]]*(today_prices %>% filter(firm == n) %>% .$price))
			}

			today_portfolio[[strategy]] <- tmp_value
		}
		portfolio <- rbind(portfolio,today_portfolio)
	}
	results <- portfolio
	portfolio_own <- stocks_owned
	return(list(results = results, portfolio = portfolio_own))
}
