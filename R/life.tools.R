Sys.time( )
Sys.timezone( )
Sys.setenv( TZ = "Europe/Berlin" )

#' calendar
#'
#' @param start first date
#' @param end last date
#' @param abbreviate if true the names of weekdays will be abbreviated
#'
#' @return a calendar from start date to end date
#' @export
#'
#' @examples
#' calendar( )
calendar <-
	function( start = "2000-01-01", end = "2017-06-19", abbreviate = T, tz = "Europe/Berlin" ) {

		dt <-
			seq( lubridate::as_date( start ), lubridate::as_date( end ), by = 1 )

		d <-
			data.frame(
				date = dt,
				day.of.week = lubridate::wday( dt ),
				day_of_week = weekdays( dt, abbreviate = abbreviate ) )

		d$date <-
			as.character( d$date )

		d }

#' some.sics
#'
#' @description some.sics creates a vector of coherently sics of same width.
#' @param n.size count of sics that shall be created
#' @param n.first first sic to be created
#' @param n.last last sic to be created
#' @param prefix a string written in front of the sic
#' @param postfix a string written behind the sic
#'
#' @return character vector of sics
#' @export
#'
#' @examples
#' some.sics( n.first = 0, n.last = 110 )
#' some.sics( n.size = 9 )
#' some.sics( 10, prefix = "LIFE" )
#'
some.sics <-
	function( n.size = 100, n.first = 1, n.last = n.first + n.size - 1, digits = 1 + floor( log10( n.last ) ), prefix = "LI", postfix = "" ) {

		paste0( prefix, stringr::str_pad( c( n.first : n.last ), digits, pad = "0" ), postfix ) }

#' IF NOT
#'
#' @description shorten for if( !... ) do something else do something different
#' @param cond condition that hast to be false
#' @param optTrue option running if condition is not TRUE
#' @param optFalse option running if condition is not FALSE
#'
#' @return execution of code optTrue or optFalse
#' @export
#'
#' @examples
#' ifnot( 1 == 2, cat( "one is not equal to two" ), cat( "one is not not equal to two" ) )
ifnot <-
	function( cond, optTrue, optFalse = { } ) {

		if( !cond ) {

			optTrue

		} else {

			optFalse } }

#' LIST APPEND
#'
#' @description list.append gives the opportunity to append elements to a list and name it.
#' @param lst list that should extended by the element x
#' @param x element that should be append to list lst
#' @param name optinal a name for list element
#'
#' @return by x extended list
#' @export
#'
#' @examples
#' ( lst <- list( x = 9 ) )
#' ( lst <- list.append( lst, value = Sys.time( ), name = "TIME" ) )
#' ( lst <- list.append( lst, "y", "Ypsilon" ) )
#' ( lst <- list.append( lst, "unnamed" ) )
#' ( lst <- list.append( lst, c( "A", "B" ), c( "a", "b" ) ) )
list.append <-
	function( list, x, name = NA ) {

		list[[ length( list ) + 1 ]] <-
				x

			if( !is.na( name ) )

				names( list )[ length( list ) ] <-
					name

		list }

#' TABLE DATA
#'
#' @param data dataframe which columns should be summarised
#'
#' @return summary of data
#' @export
#'
#' @examples
#' table.data(data.frame(x=c(1:3),y=c(NA,1,NA),z=c(NA,NA,NA)))
#' table.data(data.frame(x=c(1:3),y=c(NA,1,NA),z=c(NA,NA,NA)))
table.data <-
	function( data ) {
		
		d <-
			data[ , sapply( data, is.numeric ) ]

		missing <-
			sapply( d, function( d ) sum( is.na( d ) ) )

		available <-
			sapply( d, function( d ) sum( !is.na( d ) ) )

		min <-
			sapply( d, min, na.rm = T )

		median <-
			sapply( d, median, na.rm = T )

		mean <-
			sapply( d, mean, na.rm = T )

		max <-
			sapply( d, max, na.rm = T )

		d.num <-
			rbind( available, missing )

		d.sum <-
			rbind( min, mean, median, max )
			
		d.num.sum <-
			rbind( d.num, d.sum )
			
		d <-
			data[ , sapply( data, function( dd ) !is.numeric( dd ) ) ]
			
		missing <-
			sapply( d, function( d ) sum( is.na( d ) ) )

		available <-
			sapply( d, function( d ) sum( !is.na( d ) ) )

		d.fac <-
			rbind( available, missing )
			
		list( FACTORS = d.fac, NUMBERS = d.num.sum )
}

#' REMOVE COLUMNS
#'
#' @param data A dataframe that contains columns which should be removed.
#' @param column.names Names of columns that should be removed.
#'
#' @return A dataframe without removed columns.
#' @export
#'
#' @examples
#' remove.columns(data.frame(x="X",y="Y",z="Z",w="W"),c("x","z"))
remove.columns <-
	function( data, column.names ) {

		data[ , !names( data ) %in% column.names ]
	}
