options(repr.plot.width = 14, repr.plot.height=7)
theme_update(text = element_text(size = 20))
theme_set(theme_bw())

# function to get adjusted OHLC prices for a given symbol in a data frame using dplyr
get_ohlc <- function(symbol, start_date, end_date) {
  data <- getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  data_df <- data.frame(date = index(data), coredata(data))
  colnames(data_df) <- tolower(sub(paste0(symbol, "\\."), "", colnames(data_df)))
  data_df <- rename(data_df, "adj_close" = adjusted)

  return(data_df)
}
