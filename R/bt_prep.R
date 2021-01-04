

#' Prep data function updated for Clarifi Data
#'
#' @param b environment with symbols time series
#' @param align alignment type
#' @param dates subset of dates
#' @param fill.gaps fill gaps introduced by merging
#' @param basic control if xts object are created
#'
#' @return
#' @export
#'
#' @examples
bt_prep <- function(
  b,				# environment with symbols time series
  align = c('keep.all', 'remove.na'),	# alignment type
  dates = NULL,	# subset of dates
  fill.gaps = F,	# fill gaps introduced by merging
  basic = F		# control if xts object are created
)
{
  # setup
  if( !exists('symbolnames', b, inherits = F) ) b$symbolnames = ls(b)
  symbolnames = b$symbolnames
  nsymbols = SIT::len(symbolnames)

  if( nsymbols > 1 ) {
    # merge
    out = SIT::bt.merge(b, align, dates)

    for( i in 1:nsymbols ) {
      temp = zoo::coredata( b[[ symbolnames[i] ]] )[ out$date.map[,i],, drop = FALSE]
      b[[ symbolnames[i] ]] = SIT::iif(basic, temp, SIT::make.xts( temp, out$all.dates))

      # fill gaps logic
      map.col = SIT::find.names('Close,Volume,Open,High,Low,Adjusted', b[[ symbolnames[i] ]])
      if(fill.gaps & !is.na(map.col$Close)) {
        close = zoo::coredata(b[[ symbolnames[i] ]][,map.col$Close])
        n = SIT::len(close)
        last.n = max(which(!is.na(close)))
        close = SIT::ifna.prev(close)
        if(last.n + 5 < n) close[last.n : n] = NA
        b[[ symbolnames[i] ]][, map.col$Close] = close
        index = !is.na(close)

        if(!is.na(map.col$Volume)) {
          index1 = is.na(b[[ symbolnames[i] ]][, map.col$Volume]) & index
          b[[ symbolnames[i] ]][index1, map.col$Volume] = 0
        }

        #for(j in colnames(b[[ symbolnames[i] ]])) {
        for(field in SIT::spl('Open,High,Low')) {
          j = map.col[[field]]
          if(!is.null(j)) {
            index1 = is.na(b[[ symbolnames[i] ]][,j]) & index
            b[[ symbolnames[i] ]][index1, j] = close[index1]
          }}

        j = map.col$Adjusted
        if(!is.null(j)) {
          b[[ symbolnames[i] ]][index, j] = SIT::ifna.prev(b[[ symbolnames[i] ]][index, j])
        }


        #for(j in setdiff(1:ncol( b[[ symbolnames[i] ]] ), unlist(map.col))) {
        #	b[[ symbolnames[i] ]][index, j] = ifna.prev(b[[ symbolnames[i] ]][index, j])
        #}
      }
    }
  } else {
    if(!is.null(dates)) b[[ symbolnames[1] ]] = b[[ symbolnames[1] ]][dates,]
    out = list(all.dates = SIT::index.xts(b[[ symbolnames[1] ]]) )
    if(basic) b[[ symbolnames[1] ]] = zoo::coredata( b[[ symbolnames[1] ]] )
  }

  # dates
  b$dates = out$all.dates

  # empty matrix
  dummy.mat = matrix(double(), SIT::len(out$all.dates), nsymbols)
  colnames(dummy.mat) = symbolnames
  if(!basic) dummy.mat = SIT::make.xts(dummy.mat, out$all.dates)

  # weight matrix holds signal and weight information
  b$weight = dummy.mat

  # execution price, if null use Close
  b$execution.price = dummy.mat

  # populate prices matrix
  for( i in 1:nsymbols ) {
    if( quantmod::has.Cl( b[[ symbolnames[i] ]] ) ) {
      dummy.mat[,i] = quantmod::Cl( b[[ symbolnames[i] ]] );
    }
  }
  b$prices = dummy.mat

  #populate market cap matrix
  for (i in 1:nsymbols) {
    dummy.mat[, i] = b[[symbolnames[i]]][,paste(symbolnames[i],".MKTCAP",sep = "")]
  }
  b$MKTCAP = dummy.mat

  #populate dividends matrix
  for (i in 1:nsymbols) {
    dummy.mat[, i] = b[[symbolnames[i]]][,paste(symbolnames[i],".DVPS",sep = "")]
  }
  b$DIVPS12 = dummy.mat
}

