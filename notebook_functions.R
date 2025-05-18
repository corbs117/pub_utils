library(knitr)

options(repr.plot.width = 14, repr.plot.height=7)
theme_update(text = element_text(size = 20))
theme_set(theme_bw())

################################
# function to get adjusted OHLC prices for a given symbol in a data frame using dplyr
################################
get_ohlc <- function(symbol, start_date= '1995-01-01', end_date= Sys.Date()) {
  data <- getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  data_df <- data.frame(date = index(data), coredata(data))
  colnames(data_df) <- tolower(sub(paste0(symbol, "\\."), "", colnames(data_df)))
  data_df <- rename(data_df, "adj_close" = adjusted)

  return(data_df)
}

################################
# Get 13-week funding rates - formatted for rsims
################################
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

################################
# function for getting headline statistics from backtest results
################################
portfolio_headline_statistics <- function(backtest_results) {
  port_equity <- backtest_results %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(equity = sum(exposure)) %>%
    dplyr::select(date, equity)

  # headline stats
  stats <- port_equity %>%
      mutate(returns = log(equity/dplyr::lag(equity)),
             peak = cummax(equity),
             drawdown = (equity - peak) / peak) %>%
        summarise(
          "Ann.Return(%)" = 252*mean(returns, na.rm = TRUE)*100,
          "Ann.Volatility(%)" = sqrt(252)*sd(returns, na.rm = TRUE)*100,
          "Ann.Sharpe" = `Ann.Return(%)`/`Ann.Volatility(%)`,
          "Max Drawdown(%)" = min(drawdown, na.rm = TRUE) * 100
        )
print(kable(stats, digits = 2, format = "simple"))
    
  # equity curve
  p <- port_equity %>%
    ggplot(aes(x = date, y = equity)) +
      geom_line() +
      #scale_y_log10() +
      labs(
        x = "Date",
        y = "Equity, $",
        title = "Simple ETF RP - Equal Weight"
      )
  print(p)
}
