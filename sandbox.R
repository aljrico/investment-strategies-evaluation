library(argparse)
library(rvest)
library(tidyverse)
library(data.table)
library(quantmod)
library(magrittr)

library(Quandl)


# Argument parser for command line use
parser <- ArgumentParser()
parser$add_argument("-v", "--verbose", action = "store_true", default = TRUE,
										help = "Print extra output [default]")
parser$add_argument("--quietly", action = "store_false",
										dest = "verbose", help = "Print little output")
parser$add_argument("-f", "--file", type = "character", dest = "csv_name",
										default = "sp-500.csv",
										help = "CSV file to save data to [default: sp-500.csv]")
parser$add_argument("-s", "--sleep", type = "integer", dest = "sleeptime",
										default = 2,
										help = paste("Time (seconds) between fetching symbols",
																 "[default: 2] (don't flood websites with",
																 "requests!)"))
parser$add_argument("--inner", action = "store_true", default = FALSE,
										dest = "inner",
										help = paste("Inner join; only dates where all symbols",
																 "have data will be included"))
parser$add_argument("--start", type = "character", dest = "start",
										default = "1997-01-01",
										help = paste("Earliest date (YYYY-MM-DD) to include",
																 "[default: 1997-01-01]"))
parser$add_argument("--end", type = "character", dest = "end",
										default = "today",
										help = paste('Last date (YYYY-MM-DD or "today") to',
																 'include [default: "today"]'))
parser$add_argument("-k", "--key", type = "character", dest = "api_key",
										default = NULL,
										help = "Quandl API key, needed if getting Quandl data")
parser$add_argument("-q", "--quandl", action = "store_true", default = FALSE,
										dest = "use_quandl", help = "Get data from Quandl")
parser$add_argument("-a", "--adjust", action = "store_true", default = FALSE,
										dest = "adjust", help = "Adjust prices (Quandl only)")
parser$add_argument("--about", action = "store_true", default = FALSE,
										dest = "about",
										help = paste("Print information about the script and its",
																 "usage, then quit"))

args <- parser$parse_args()

join <- "outer"
if (args$inner) {
	join <- "inner"
}
verbose <- args$verbose
start <- args$start
if (args$end == "today") {
	end <- Sys.Date()
} else {
	end <- args$end
}
sleeptime <- args$sleeptime  # In seconds
csv_name <- args$csv_name
api_key <- args$api_key
use_quandl <- args$use_quandl
adjust <- args$adjust
about <- args$about

if (about) {
	# Display a message, then quit
	comm_name <- substring(commandArgs(trailingOnly = FALSE)[4], 8)
	cat(comm_name, "\n(c) 2017 Curtis Miller\n",
			"Licensed under GNU GPL v. 3.0 available at ",
			"https://www.gnu.org/licenses/gpl-3.0.en.html \n",
			"E-mail: cgmil@msn.com\n\n",
			"This script fetches closing price data for ticker symbols included ",
			"in the S&P 500 stock index. A list of symbols included in the index ",
			"is fetched from this webpage:",
			"https://en.wikipedia.org/wiki/List_of_S%26P_500_companies  The list ",
			"is parsed and the symbols included in the list are fetched from ",
			"either Google Finance (the default) or Quandl (which requires a ",
			"Quandl API key). If Quandl is the data source, adjusted data can be ",
			"fetched instead. The resulting data set is then saved to a CSV",
			"file in the current working directory.\n\n",
			"This package requires the following R packages be installed in order ",
			"to work (all of which are available from CRAN and can be downloaded ",
			"and installed automatically from R via the command ",
			"'install.packages(\"package_name\")'):\n\n",
			"* rvest\n",
			"* magrittr\n",
			"* quantmod\n",
			"* Quandl\n",
			"* tibble\n",
			"* argparse (used only for the command line interface)\n\n",
			"This script was written by Curtis Miller and was made available on ",
			"his website: https://ntguardian.wordpress.com\n\n",
			"You can read more about this script in the following article: ",
			"https://ntguardian.wordpress.com/2017/07/10/downloading-sp-500-stock-data-google-quandl-r-command-line-script/#more-3102\n\n", sep = "")
	quit()
}

start %<>% as.Date
end %<>% as.Date

options("getSymbols.warning4.0"=FALSE)

sp500_wiki <- read_html(
	"https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")

symbols_table <- sp500_wiki %>%
	html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
	html_table()

symbols_table <- symbols_table[[1]]
symbols <- as.character(symbols_table$`Ticker symbol`)

if (use_quandl) {
	Quandl.api_key(api_key)
}

sp500 <- NULL
for (s in symbols) {
	Sys.sleep(sleeptime)
	if (verbose) {
		cat("Processing:", s, "...")
	}
	tryCatch({
		if (use_quandl) {
			s_data <- Quandl.datatable("WIKI/PRICES", ticker = c(s),
																 date.gte = start, date.lte = end)
			rownames(s_data) <- as.Date(s_data$date)
			if (adjust) {
				s_data <- s_data[, "adj_close", drop = FALSE]
			} else {
				s_data <- s_data[, "close", drop = FALSE]
			}
		} else {
			s_data <- Cl(getSymbols(s, src="google", from = start, to = end,
															env = NULL))
		}
		names(s_data) <- s
		s_data %<>% as.xts
		if (length(unique(s_data)) > 1) {    # Don't allow what is effectively
			# empty data
			if (is.null(sp500)) {
				sp500 <- as.xts(s_data)
			} else {
				sp500 %<>% merge(s_data, join = join)
			}
			if (verbose) {
				cat(" Got it! From", start(s_data) %>% as.character, "to",
						end(s_data) %>% as.character, "\n")
			}
		} else if (verbose) {
			cat("Sorry, but not this one!\n")
		}
	}, error = function(e) {
		if (verbose) {
			cat("Sorry, but not this one!\n")
		}
	})
}

badsymbols <- setdiff(symbols, names(sp500))
if (verbose & (length(badsymbols) > 0)) {
	cat("There were", length(badsymbols),
			"symbols for which data could not be obtained.\nThey are:", badsymbols,
			"\n")
}




library(BatchGetSymbols)

first.date <- Sys.Date()-365
last.date <- Sys.Date()

df.SP500 <- GetSP500Stocks()
tickers <- df.SP500$tickers

l.out <- BatchGetSymbols(tickers = tickers,
												 first.date = first.date,
												 last.date = last.date)

print(l.out$df.control)
print(l.out$df.tickers)

head(l.out)
