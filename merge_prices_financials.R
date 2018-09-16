library(zoo)
source("get_data.R")



stocks <- unique(all_prices$firm)
tidy_merged <- tibble()
merged <- left_join(all_prices,fins)
for(s in stocks){
	df <- merged %>% filter(firm == s)
	df <- na.locf(df) %>%
		na.omit() %>%
		distinct() %>%
		as_tibble()
	tidy_merged <- tidy_merged %>% rbind(tidy_merged,df)
}

tidy_merged$date <- as.Date(tidy_merged$date)
tidy_merged$firm <- as.factor(tidy_merged$firm)
cols <- colnames(tidy_merged)
for(c in cols) if(is.character(tidy_merged[[c]])) tidy_merged[[c]] <- as.numeric(tidy_merged[[c]])

tidy_merged <- tidy_merged %>%  distinct()

