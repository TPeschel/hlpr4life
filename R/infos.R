#' TABLE COLUMNS
#'
#' @param data dataframe which columns should be summarised
#' @param show.missings flag for summing missings or availables
#'
#' @return print of missings or availables, depending on show.missings
#' @export
#'
#' @examples
#' table.columns(data.frame(x=c(1:3),y=c(NA,1,NA),z=c(NA,NA,NA)))
#' table.columns(data.frame(x=c(1:3),y=c(NA,1,NA),z=c(NA,NA,NA)),F)
table.columns <-
	function( data, show.missings = T ) {
		if( show.missings )
			return( sapply( data, function( d ) sum( is.na( d ) ) ) )
		else
			return( sapply( data, function( d ) sum( !is.na( d ) ) ) ) }
