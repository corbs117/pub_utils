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

   # exposure over time
  p <- backtest_results %>%
  select(ticker, date, exposure) %>%
  ggplot(aes(x = date, y = exposure, fill = ticker, colour = ticker)) +
    geom_bar(stat = "identity") +
    #scale_y_log10() +
    labs(
      x = "Date",
      y = "Exposure, $",
      title = "Exposure by ticker"
    )
  print(p)

  p <- port_equity %>%
    mutate(returns = log(equity/dplyr::lag(equity))) %>%
    na.omit() %>%
    mutate(
    `Rolling Ann.Return` = 252*roll::roll_mean(returns, width = 252, min_obs = 252),
    `Rolling Ann.Volatility` = sqrt(252)*roll::roll_sd(returns, width = 252, min_obs = 252),
    `Rolling Ann.Sharpe` = `Rolling Ann.Return`/`Rolling Ann.Volatility`
    ) %>%
    select(date, `Rolling Ann.Return`, `Rolling Ann.Volatility`, `Rolling Ann.Sharpe`) %>%
    pivot_longer(cols = c(-date), names_to = "metric", values_to = "value") %>%
    mutate(metric = factor(metric, levels = c("Rolling Ann.Return", "Rolling Ann.Volatility", "Rolling Ann.Sharpe"))) %>%
    ggplot(aes(x = date, y = value)) +
    geom_line() +
    facet_wrap(~metric, ncol = 1, scales = "free_y") +
    labs(
      title = "Rolling performance",
      x = "Date",
      y = "Value"
    )
   print(p)
}

################################
# various cost statys
################################
portfolio_costs <- function(backtest_results) {
  # trading costs through time
  p <- backtest_results %>%
    select(ticker, date, commission) %>%
    filter(ticker != "Cash") %>%
    ggplot(aes(x = date, y = commission, colour = ticker, fill = ticker)) +
      geom_bar(stat = "identity") +
      labs(
        x = "Date",
        y = "Commission, $",
        title = "Commission by ticker"
      )
  print(p)

  p <- results_df %>%
  ggplot(aes(x = date, y = share_trades)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Number of shares traded",
    x = "Date",
    y = "Number of shares traded"
  )
  print(p)
}

################################
# portfolio sharpe on backtest results - mainly for parameter seeking
################################
calculate_portfolio_sharpe <- function(df) {
  # Step 1: Aggregate exposure per day
  portfolio_equity <- df %>%
    group_by(date) %>%
    summarise(total_equity = sum(exposure, na.rm = TRUE), .groups = "drop") %>%
    arrange(date)
  
  # Step 2: Compute daily log returns
  portfolio_returns <- portfolio_equity %>%
    mutate(log_return = log(total_equity / dplyr::lag(total_equity))) %>%
    filter(!is.na(log_return))
  
  # Step 3: Compute Sharpe ratio
  daily_excess_returns <- portfolio_returns$log_return 
  mean_daily <- mean(daily_excess_returns)
  sd_daily <- sd(daily_excess_returns)
  sharpe_ratio <- mean_daily / sd_daily * sqrt(252)
  
  return(sharpe_ratio)
}

################################
# function for getting headline statistics from backtest results
# THIS DOESNT WORK YET
################################
portfolio_headline_statistics_NAV <- function(backtest_results) {
# plot NAV over exposure
NAV <- results_df %>%
  group_by(date) %>%
  summarise(exposure = sum(exposure)) %>%
  mutate(ticker = "NAV")

NAV <- results_df %>%
  select(ticker, date, exposure) %>%
  bind_rows(NAV) %>%
  arrange(date, ticker)

NAV %>%
    filter(ticker != "NAV") %>%
    ggplot(aes(x = date, y = exposure, fill = ticker)) +
    geom_area() +
    scale_fill_manual(name = "ticker", values = c('#00b0f6', '#F8766D'), limits = c('TLT', 'Cash')) +
    geom_line(data = NAV %>% dplyr::filter(ticker == "NAV"), aes(x = date, y = exposure, colour = "ticker"), linewidth = 1.5) +
    scale_colour_manual(values = "black", labels = "NAV") +
    labs(
      x = "Date",
      y = "Expsoure",
      title = "Dollar exposure and Net Asset Value",
      colour = ""
    )
}
