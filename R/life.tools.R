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
#' @param lst list that should extended by the element x
#' @param x element that should be append to list lst
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
	function( list, value, name = NA ) {

		if( is.na( name ) && length( value ) != length( value ) ) {

			print( "Error! Value and name differ in their lengths." )

				return( NA ) }

		for( i in 1 : length( value ) ) {

			list[[ length( list ) + 1 ]] <-
				value[ i ]

			if( !is.na( name[ i ] ) )

				names( list )[ length( list ) ] <-
					name[ i ]
		}

		list }
