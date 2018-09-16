library(tidyverse)
library(zoo)


getMerge <- function(prices, financials){

	# Get common days
	k <- 1
	absences <- which(!(financials$date %in% prices$date))
	while(sum(!(unique(financials$date) %in% unique(prices$date))) > 0){
		count <- sum(!(unique(financials$date) %in% unique(prices$date)))
		financials$date[[absences[k]]] <- financials$date[[absences[k]]] + 1
		if(sum(!(unique(financials$date) %in% unique(prices$date))) < count) k <- k + 1
	}

	stocks <- unique(prices$firm)
	tidy_merged <- tibble()
	merged <- left_join(prices,financials, by = c("date", "firm"))
	for(s in stocks){
		df <- merged %>% filter(firm == s)
		tidy_merged <- df %>%
			na.locf() %>%
			na.omit() %>%
			distinct() %>%
			as_tibble() %>%
			rbind(tidy_merged)
	}

	tidy_merged$date <- as.Date(tidy_merged$date)
	tidy_merged$firm <- as.factor(tidy_merged$firm)
	cols <- colnames(tidy_merged)
	for(c in cols) if(is.character(tidy_merged[[c]])) tidy_merged[[c]] <- as.numeric(tidy_merged[[c]])

	tidy_merged <- tidy_merged %>%  distinct()

	return(tidy_merged)
}
