library(tidyverse)
library(data.table)
library(quantmod)
library(rvest)


# Get List of Securities --------------------------------------------------

getTickers <- function(index = "sp500"){

	if(index == "sp500"){
		sp500_wiki <- read_html(
			"https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")

		symbols_table <- sp500_wiki %>%
			html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
			html_table()

		symbols_table <- symbols_table[[1]]
		tickers <- as.character(symbols_table$`Ticker symbol`)
	}else{
		stop("Index not defined yet")
	}

	for(i in 1:length(tickers)) tickers[[i]] <- gsub('\\.', '-', tickers[[i]])

	return(tickers)
}

tickets <- getTickets()


# Get Historical Prices ---------------------------------------------------

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

all_prices %>% data.table()



# Get Financials ----------------------------------------------------------

get_financials <- function(stock){
	require(rvest)
	require(data.table)
	require(tidyverse)
	require(progress)

	output <- tibble()
	pb <- progress_bar$new(total = length(stock))

	for (i in 1:length(stock)) {
		pb$tick()
		tryCatch(
			{
				url <- "https://finance.yahoo.com/quote/"
				url <- paste0(url,stock[i],"/financials?p=",stock[i])
				wahis.session <- html_session(url)
				p <-    wahis.session %>%
					html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table')%>%
					html_table(fill = TRUE)
				IS <- p[[1]]
				colnames(IS) <- paste(IS[1,])
				IS <- IS[-c(1,5,12,20,25),]
				names_row <- paste(IS[,1])
				IS <- IS[,-1]
				IS <- apply(IS,2,function(x){gsub(",","",x)})
				IS <- as.data.frame(apply(IS,2,as.numeric))
				rownames(IS) <- paste(names_row)
				temp1 <- IS
				url <- "https://finance.yahoo.com/quote/"
				url <- paste0(url,stock[i],"/balance-sheet?p=",stock[i])
				wahis.session <- html_session(url)
				p <-    wahis.session %>%
					html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table')%>%
					html_table(fill = TRUE)
				BS <- p[[1]]
				colnames(BS) <- BS[1,]
				BS <- BS[-c(1,2,17,28),]
				names_row <- BS[,1]
				BS <- BS[,-1]
				BS <- apply(BS,2,function(x){gsub(",","",x)})
				BS <- as.data.frame(apply(BS,2,as.numeric))
				rownames(BS) <- paste(names_row)
				temp2 <- BS
				url <- "https://finance.yahoo.com/quote/"
				url <- paste0(url,stock[i],"/cash-flow?p=",stock[i])
				wahis.session <- html_session(url)
				p <-    wahis.session %>%
					html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table')%>%
					html_table(fill = TRUE)
				CF <- p[[1]]
				colnames(CF) <- CF[1,]
				CF <- CF[-c(1,3,11,16),]
				names_row <- CF[,1]
				CF <- CF[,-1]
				CF <- apply(CF,2,function(x){gsub(",","",x)})
				CF <- as.data.frame(apply(CF,2,as.numeric))
				rownames(CF) <- paste(names_row)
				temp3 <- CF
				#assign(paste0(stock[i],'.f'),value = list(IS = temp1,BS = temp2,CF = temp3),envir = parent.frame())
				total_df <- as.data.frame(rbind(temp1,temp2,temp3))
				total_df$value <- rownames(total_df)
				for(k in 1:length(total_df$value)) total_df$value[[k]] <- gsub(" ", "",total_df$value[[k]])
				financial_camps <- total_df$value
				df <- total_df %>% t()
				df <- df[!rownames(df) %in% c("value"),]
				dates <- rownames(df)
				df <- df %>% data.table()
				colnames(df) <- financial_camps
				df$dates <- dates
				df$company <- stock[[i]]
			},
			error = function(cond){
				message(stock[i], "Give error ",cond)
			}
		)
		output <- rbind(output, df)
	}
	return(output)
}

fins <- get_financials(tickers)

