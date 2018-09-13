
getFinancials <- function(stock){
	require(rvest)
	require(data.table)
	require(tidyverse)
	require(progress)
	require(lubridate)

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
				df$date <- mdy(dates)
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
