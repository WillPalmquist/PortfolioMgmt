#' Month Fill function updated for Clarifi Data
#'
#' the month fill function will convert quarterly fundamental data
#' to monthly by filling reported financials forward
#'
#' @param b environment with symbols time series
#' @param metric user specified financial metric named according to Clarifi export
#'
#' @return
#' @export
#'
#' @examples
bt_fillmonths <- function(
  b,			# environment with symbols time series
  metric	# user specified financial metric
)
{
  out = xts::xts()
  symbolnames = b$symbolnames
  nsymbols = length(symbolnames)
  for( i in 1:nsymbols) {
    tick = symbolnames[i]
    temp = b[[tick]][,metric]
    l = cumsum(!is.na(temp))
    x = c(NA, temp[!is.na(temp)])[replace(l, stats::ave(l, l, FUN = seq_along) > 13, 0) + 1]
    temp[,1] = x
    names(temp) = tick
    out = cbind(out,temp)
  }
  return(out)
}
