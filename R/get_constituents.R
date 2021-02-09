#' Get the constituents of a particular quantile for each date in backtest
#'
#' @param model_quantile single model quantile object
#'
#' @return list of tickers for each date in backtest
#' @export
#'
#' @examples
get_constituents <- function(model_quantile) #model backtest object
{
  const_lists <- list()
  for (i in 1:nrow(model_quantile$weight)){
    df <- data.frame(t(model_quantile$weight[i]))
    colnames(df) <- "weight"
    ticks <- df %>%
      filter(weight != 0) %>%
      rownames()
    name <- gsub("-","_",substr(index(models$Q5$weight[i]),0,10))
    const_lists[[paste(name)]] <- ticks
    #const_lists <- append(const_lists,temp_list)
  }
  return(const_lists)
}
