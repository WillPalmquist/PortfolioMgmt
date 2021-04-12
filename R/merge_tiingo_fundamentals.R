#' Title
#'
#' @param data a data.frame of company financial statements retrieved from `riingo_fundamental_statements()`
#'
#' @return data.frame of quarterly fundamentals
#' @export
#'
#' @examples
merge_tiingo_fundamentals <- function(data){
  data <- data %>%
    filter(.,quarter != 0)
  df_all <- NULL
  for (j in c("overview","balanceSheet","incomeStatement", "cashFlow")){
    rows <- sort(as.vector(data[[j]][[1]][["dataCode"]]))
    df <- as.data.frame(matrix(nrow=length(rows),ncol = length(data[[j]])))
    rownames(df)<-rows
    if(j == "overview") {
      df["ticker",] = data$ticker
      df["quarter",] = data$quarter
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
  x <- c("ticker","quarter_end_date","quarter")
  df_all <- rbind(df_all[x,], df_all[!rownames(df)%in%x,])
  return(df_all)
}
