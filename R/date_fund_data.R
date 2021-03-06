#' Date function for fundamental data
#'
#' @param data fundamental data stored in environment variable
#'
#' @return stored date object
#' @export
#'
#' @examples
date_fund_data <- function(data)
{
  # construct date
  temp = as.character(data[1,])
  quarter.end.date = as.Date(paste(substr(temp,0,7), '-01', sep=''))
  quarterly.indicator = data['quarterly indicator',]


  months = seq(quarter.end.date[1], utils::tail(quarter.end.date,1)+365, by='1 month')
  index = match(quarter.end.date, months)
  quarter.end.date = months[ SIT::iif(quarterly.indicator == '4', index+2, index+2) + 1 ] - 1

  fund.date = quarter.end.date

  return(fund.date)
}
