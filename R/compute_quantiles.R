
#' Compute Quantiles updated for Clarifi Data
#'
#' @param data factor time series
#' @param next.month.ret future returns
#' @param smain plot title
#' @param n.quantiles number of quantiles
#' @param plot flag to create plot
#'
#' @return list with returns, weights, quantiles, ranks
#' @export
#'
#' @examples
compute_quantiles <- function(
  data, 			# factor
  next.month.ret, # future returns
  smain='', 		# title for plot
  n.quantiles=5, 	# number of quantiles
  plot=T			# flag to create plot
)
{
  n = ncol(data)
  nperiods = nrow(data)

  data = coredata(ifna(data,NA))
  next.month.ret = coredata(ifna(next.month.ret,NA))

  temp = matrix(NA, nperiods, n.quantiles)
  hist.factor.quantiles = hist.ret.quantiles = temp

  temp = matrix(NA, nperiods, n)
  quantiles = weights = ranking = temp

  #index = which(rowSums(!is.na(data * next.month.ret)) > n/2)
  #index = which(rowSums(!is.na(data)) > n/2)
  index = which(rowSums(!is.na(data)) >= n.quantiles)
  for(t in index) {
    factor = data[t,]
    ret = next.month.ret[t,]

    ranking[t,] = rank(-factor, na.last = 'keep','first')
    t.ranking = ceiling(n.quantiles * ranking[t,] / count(factor))

    quantiles[t,] = t.ranking
    weights[t,] = 1/tapply(rep(1,n), t.ranking, sum)[t.ranking]

    hist.factor.quantiles[t,] = tapply(factor, t.ranking, mean)
    hist.ret.quantiles[t,] = tapply(ret, t.ranking, mean,na.rm = T)
  }

  # create plot
  if(plot) {
    par(mar=c(4,4,2,1))
    temp = 100*apply(hist.ret.quantiles,2,mean,na.rm=T)
    barplot(temp, names.arg=paste(1:n.quantiles), ylab='%',
            main=paste(smain, ', spread =',round(temp[1]-temp[n.quantiles],2), '%'))
  }

  return(list(quantiles=quantiles, weights=weights, ranking=ranking,
              hist.factor.quantiles = hist.factor.quantiles, hist.ret.quantiles = hist.ret.quantiles))
}
