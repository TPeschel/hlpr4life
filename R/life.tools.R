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
				week.day = lubridate::wday( dt ),
				week.day.name = weekdays( dt, abbreviate = abbreviate ) )

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
