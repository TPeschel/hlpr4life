Sys.time( )
Sys.timezone( )
Sys.setenv( TZ = "BMT" )

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
	function( start = "2000-01-01 00:00:00", end = "2017-06-19 00:00:00", abbreviate = T ) {

		dt <-
			as.POSIXct( seq( lubridate::as_datetime( start ), lubridate::as_datetime( end ), by = 24 * 3600 ), tz = "BMT" )

		d <-
			data.frame(
				date = dt,
				week.days = lubridate::wday( dt ),
				week.days.name = weekdays( dt, abbreviate = abbreviate ) )

		d$date <-
			as.character( d$date )

		d }

#' make.sics
#'
#' @param n count of sics that shall be created
#'
#' @return vector of characters of sics
#' @export
#'
#' @examples
#' make.sics( 90, 110 )
make.sics <-
	function( n.size = 100, n.first = 1, n.last = n.size ) {

		sc <-
			sapply(
				as.character( c( n.first : n.last ) ),
				function( s ) {
					paste0(
						paste(
							rep(
								"0",
								times = floor( log10( n.last ) + 1 ) - nchar( s ) ),
							collapse = "" ),
						s ) } )
		names( sc ) <-
			NULL
		sc }
