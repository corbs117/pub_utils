options(repr.plot.width = 14, repr.plot.height=7)
theme_update(text = element_text(size = 20))
theme_set(theme_bw())

# function to get adjusted OHLC prices for a given symbol in a data frame using dplyr
get_ohlc <- function(symbol, start_date= '1995-01-01', end_date= Sys.Date()) {
  data <- getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  data_df <- data.frame(date = index(data), coredata(data))
  colnames(data_df) <- tolower(sub(paste0(symbol, "\\."), "", colnames(data_df)))
  data_df <- rename(data_df, "adj_close" = adjusted)

  return(data_df)
}


# Get 13-week funding rates - formatted for rsims
get_rates <- function(symbol = '^IRX', start_date= '1995-01-01', end_date= Sys.Date()) {

  rates <- get_ohlc(symbol, start_date, end_date)
  rates <- rates %>%
    select(date, adj_close) %>%
    rename(rate = adj_close)

  rates <- rates %>%
  tidyr::fill(rate, .direction = "down")

  # Show rows with NA values
  print(rates[!complete.cases(rates), ])

  # convert to a daily percentage
  broker_spread <- 0.5  # %
  rates <- rates %>%
    mutate(applied_rate = pmax((rate - broker_spread)/(365*100), 0))

  # plot it to make sure it looks sensible
  rates %>%
    ggplot(aes(x = date, y = applied_rate*100*365)) +
    geom_line() +
    labs(x = "Date", y = "Applied Rate", title = "Interest earned on spare cash")


  # convert to matrix for simulation
  sim_rates <- rates %>%
    select(date, applied_rate) %>%
    data.matrix()

  return(sim_rates)
}
