#' Prep fundamental or pricing data from Tiingo API
#'
#' @param data a data.frame of company financial statements retrieved from `riingo_fundamental_statements()` or pricing data from `riingo_prices()`
#'
#' @return data.frame of combined quarterly fundamentals
#' @export
#'
#' @examples
tiingo_prep <- function(data, #data from tiingo
                        type = c("fundamental","price")){
  if (type == "fundamental") {
  data <- data %>%
    filter(.,quarter != 0)
  df_all <- NULL
  for (j in c("overview","balanceSheet","incomeStatement", "cashFlow")){
    rows <- sort(as.vector(data[[j]][[1]][["dataCode"]]))
    df <- as.data.frame(matrix(nrow=length(rows),ncol = length(data[[j]])))
    rownames(df)<-rows
    if(j == "overview") {
      df["ticker",] = data$ticker
      df["quarterly indicator",] = data$quarter
      df["quarter_end_date",] = as.character(data$date)
    }
    for (i in 1:nrow(data)){
      temp <- data[[j]][[i]]
      b <- temp["value"]
      rownames(b) <- temp$dataCode
      b <- b[order(row.names(b)),,drop = F]
      df[1:nrow(b),i] <- b
    }
    df_all <- rbind(df_all,df)
  }
  #reorder rows
  x <- c("quarter_end_date","ticker","quarterly indicator")
  df_all <- rbind(df_all[x,], df_all[!rownames(df)%in%x,])
  df_all <- df_all[!rownames(df_all) %in% c("ticker1","quarterly indicator1","quarter_end_date1"),]
  df_all <- df_all[,order(ncol(df_all):1)]
  return(df_all)
  }
  else if (type == "price") {
    data$date <- as.Date(data$date, "%Y/%m/%d")
    data <- xts(data[, 3:ncol(data)], order.by = data$date)
    return(data)
  }
}
