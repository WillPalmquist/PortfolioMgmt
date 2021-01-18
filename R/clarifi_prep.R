
#' Prep Exported Data from Clarifi
#'
#' @param clarifi_data Clarifi exported data read into a data.frame. Should have date,sector,ticker, and quarterly indicator in first four columns in that order, followed by any order of fundamental metrics. For pricing data, should have date, ticker, high, low, close, volume, dividends per share, and market capitalization.
#' @param type type of export data either "fundamental" or "price"
#'
#' @return environment variable for fundamental or pricing data.
#' @export
#'
#' @examples
clarifi_prep <- function(clarifi_data,#clarifi export data
                         type = c("fundamental","price")
)
{
  if (type == "fundamental") {
    names(clarifi_data) <- sub("\\..*", '', names(clarifi_data))
    names(clarifi_data)[1:4] <-
      c("quarter_end_date",
        "sector",
        "ticker",
        "quarterly indicator")
    tickers <- unique(clarifi_data$ticker)
    data.fund <- new.env()
    for (i in tickers) {
      # reorder dates for tickers and sort prior to backtesting
      df <- clarifi_data %>%
        filter(ticker == i)
      df$quarter_end_date <- as.Date(df$quarter_end_date, "%Y/%m/%d")
      df <- df[order(df$quarter_end_date), ] %>%
        t()
      data.fund[[i]] <- df
    }
    return(data.fund)
  }
  else if (type == "price") {
    names(clarifi_data) <-
      c("Date",
        "Ticker",
        "High",
        "Low",
        "Close",
        "Volume",
        "DVPS",
        "MKTCAP")

    tickers <- unique(clarifi_data$Ticker)
    data <- new.env()
    for (i in tickers) {
      # convert pricing to xts object
      df <- clarifi_data %>%
        filter(Ticker == i)
      df$Date <- as.Date(df$Date, "%Y/%m/%d")
      df <- xts(df[, 3:ncol(df)], order.by = df$Date)
      names(df) <- paste(i, names(df), sep = ".")
      data[[i]] <- df

    }
    return(data)
  }
}
