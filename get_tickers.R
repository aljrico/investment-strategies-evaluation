getTickers <- function(index = "sp500"){

	if(index == "sp500"){
		sp500_wiki <- read_html(
			"https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")

		symbols_table <- sp500_wiki %>%
			html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
			html_table()

		symbols_table <- symbols_table[[1]]
		tickers <- as.character(symbols_table$`Ticker symbol`)
	}

	if(index == "sp400"){
		sp400_wiki <- read_html(
			"https://en.wikipedia.org/wiki/List_of_S%26P_400_companies")

		symbols_table <- sp400_wiki %>%
			html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
			html_table()

		symbols_table <- symbols_table[[1]]
		tickers <- as.character(symbols_table$`Ticker Symbol`)
	}

	if(index == "sp1000"){
		sp1000_wiki <- read_html(
			"https://en.wikipedia.org/wiki/List_of_S%26P_1000_companies")

		symbols_table <- sp1000_wiki %>%
			html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>%
			html_table()

		symbols_table <- symbols_table[[1]]
		tickers <- as.character(symbols_table$`Ticker Symbol`)
	}

	if(index == "all"){
		sp500_wiki <- read_html(
			"https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")

		symbols_table <- sp500_wiki %>%
			html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
			html_table()

		symbols_table <- symbols_table[[1]]
		tickers <- as.character(symbols_table$`Ticker symbol`)

		sp1000_wiki <- read_html(
			"https://en.wikipedia.org/wiki/List_of_S%26P_1000_companies")

		symbols_table <- sp1000_wiki %>%
			html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>%
			html_table()

		symbols_table <- symbols_table[[1]]
		tickers2 <- as.character(symbols_table$`Ticker Symbol`)

		tickers <- c(tickers,tickers2)
	}

	for(i in 1:length(tickers)) tickers[[i]] <- gsub('\\.', '-', tickers[[i]])

	return(tickers)
}
