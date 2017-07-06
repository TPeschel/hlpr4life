rm( list = ls( ) )

library( ggplot2 )
library( lubridate )
library( dplyr )

merge.likely <-
	function(
		df.1,
		df.2,
		by.exact = NULL,
		by.exact.x = by.exact,
		by.exact.y = by.exact,
		by.likely = NULL,
		by.likely.x = by.likely,
		by.likely.y = by.likely,
		inter.vals,
		trim = F,
		reorder.names = F ) {
		#rm( list = ls())
		# df.1 = d1
		# df.2 = d2
		# by.exact.x = c( "SIC" )
		# by.exact.y = c( "SIC" )
		# by.likely.x = c( "EDAT", "Z" )
		# by.likely.y = c( "time", "Z" )
		# inter.vals = list( 15, .5 )
		# trim = T
		# reorder.names = T
		d <-
			merge(
				df.1,
				df.2,
				by.x = by.exact.x,
				by.y = by.exact.y )
		by <-
			intersect( by.likely.x, by.likely.y )
		by.likely.x <-
			setdiff( by.likely.x, by )
		by.likely.y <-
			setdiff( by.likely.y, by )
		if( 0 < length( by ) ) {
			by.likely.x <-
				c( by.likely.x, paste0( by, ".x" ) )
			by.likely.y <-
				c( by.likely.y, paste0( by, ".y" ) ) }
		if( 0 < length( by.likely.x ) ) {
			for( i in c( 1 : length( by.likely.x ) ) ) {
				if( is.numeric( d[ , by.likely.x[ i ] ] ) && is.numeric( d[ , by.likely.y[ i ] ] ) ) {
					d <-
						d[ abs( d[ , by.likely.x[ i ] ] - d[ , by.likely.y[ i ] ] ) < inter.vals[ i ], ]
					if( trim ) {
						d[ , by.likely.y[ i ] ] <-
							NULL
						names( d )[ names( d ) == by.likely.x[ i ] ] <-
							sub( "(\\w*).x", "\\1", by.likely.x[ i ], perl = T )
					}
				} else {
					if( ( is.Date( d[ , by.likely.x[ i ] ] ) && is.Date( d[ , by.likely.y[ i ] ] ) ) ||
						( is.POSIXct( d[ , by.likely.x[ i ] ] ) && is.POSIXct( d[ , by.likely.y[ i ] ] ) ) ) {
						d <-
							d[ abs( difftime( d[ , by.likely.x[ i ] ], d[ , by.likely.y[ i ] ], units = "days" ) ) <= inter.vals[ i ], ]
						if( trim ) {
							d[ , by.likely.y[ i ] ] <-
								NULL
							names( d )[ names( d ) == by.likely.x[ i ] ] <-
								sub( "(\\w*).x", "\\1", by.likely.x[ i ], perl = T )
						}
					}
				}
			}
			if( reorder.names && !trim ) {
				d.n <-
					unlist(
						mapply( list,
							by.likely.x,
							by.likely.y ) )
				d <-
					d[ , c( setdiff( names( d ), d.n ), d.n ) ]
			}
		}
		d
	}

Sys.time( )
Sys.timezone( )
Sys.setenv( TZ = "BMT" )

calendar <-
	function( start = "2010-01-01 00:00:00", end = "2017-06-19 00:00:00" ) as.POSIXct( seq( as_datetime( start ), as_datetime( end ), by = 24 * 3600 ), tz = "BMT" )

clndr <-
	calendar( "2010-01-01", "2017-06-19" )

week.days <-
	clndr[ wday( clndr ) %in% c( 1 : 5 ) ]

make.sics <-
	function( n ) {
		sc <-
			sapply(
				as.character( c( 1 : n ) ),
				function( s ) {
					paste0(
						paste(
							rep(
								"0",
								times = floor( log10( n ) + 1 ) - nchar( s ) ),
							collapse = "" ),
						s ) } )
		names( sc ) <-
			NULL
		sc }

d1 <-
	data.frame(
		sic  = sample( make.sics( 30 ), 300, T ),
		edat = as.Date( sample( clndr, 300 ) ),
		H    = round( rnorm( 300, 180, 10 ) ))

d2 <-
	data.frame(
		sic = sample( make.sics( 30 ), 300, T ),
		edat = as.Date( sample( clndr, 300 ) ),
		height = round( rnorm( 300, 180, 10 ) ) )

d <-
	merge.likely(
		d1,
		d2,
		by.exact    = c( "sic" ),
		by.likely   = c( "edat" ),
		by.likely.x = c( "H" ),
		by.likely.y = c( "height" ),
		inter.vals  = c( 12, 15 ),
		trim = F,
		reorder.names = T )

d
( d1.t <- as.data.frame( table( ( d1 %>% group_by( sic ) %>% summarise( n = n( ) ) )$n ) ) ); d1.t$Group <- 1
( d2.t <- as.data.frame( table( ( d2 %>% group_by( sic ) %>% summarise( n = n( ) ) )$n ) ) ); d2.t$Group <- 2; d2.t$Group <- as.factor( d2.t$Group )
ggplot( rbind( d1.t, d2.t ), aes( Var1, Freq, fill = Group ) ) + geom_histogram( stat = "identity" ) + theme_bw( ) + facet_grid( ~ Group ) + scale_fill_manual( values = c( "deeppink1", "deepskyblue1") )
#
# tapply( d$sic, INDEX = d1$sic, FUN = length )
# starwars %>% arrange(desc(mass))
#
# require(stats)
# g<-data.frame( groups = as.factor(rbinom(32, n = 50, prob = 0.4)) )
# tapply(g$groups, g$groups, length) #- is almost the same as
# table(groups)
#
#
# require(stats)
# groups <- as.factor(rbinom(32, n = 5, prob = 0.4))
# tapply(groups, groups, length) #- is almost the same as
# table(groups)
#
# ## contingency table from data.frame : array with named dimnames
# tapply(warpbreaks$breaks, warpbreaks[,-1], sum)
# tapply(warpbreaks$breaks, warpbreaks[, 3, drop = FALSE], sum)
#
# n <- 18; fac <- factor(rep_len(1:3, n), levels = 1:5)
# table(fac)
# tapply(1:n, fac, sum)
# as.numeric(fac)*c(1:n)
#
# addmargins(table(c(1:n)*fac))
#
# tapply(1:n, fac, sum, default = 0) # maybe more desirable
# tapply(1:n, fac, sum, simplify = FALSE)
# tapply(1:n, fac, range)
# tapply(1:n, fac, quantile)
# tapply(1:n, fac, length) ## NA's
# tapply(1:n, fac, length, default = 0) # == table(fac)
#
# ## example of ... argument: find quarterly means
# tapply(presidents, cycle(presidents), mean, na.rm = TRUE)
#
# ind <- list(c(1, 2, 2), c("A", "A", "B"))
# table(ind)
# tapply(1:3, ind) #-> the split vector
# tapply(1:3, ind, sum)
#
# ## Some assertions (not held by all patch propsals):
# nq <- names(quantile(1:5))
# stopifnot(
# 	identical(tapply(1:3, ind), c(1L, 2L, 4L)),
# 	identical(tapply(1:3, ind, sum),
# 			  matrix(c(1L, 2L, NA, 3L), 2, dimnames = list(c("1", "2"), c("A", "B")))),
# 	identical(tapply(1:n, fac, quantile)[-1],
# 			  array(list(`2` = structure(c(2, 5.75, 9.5, 13.25, 17), .Names = nq),
# 			  		   `3` = structure(c(3, 6, 9, 12, 15), .Names = nq),
# 			  		   `4` = NULL, `5` = NULL), dim=4, dimnames=list(as.character(2:5)))))
#
#

ml <-
	function(
		d1,
		d2,
		by = NULL,
		by.x = by,
		by.y = by,
		by.lk = NULL,
		by.x.lk = by.lk,
		by.y.lk = by.lk,
		min = NULL,
		max = NULL,
		reorder.names = T,
		trim = F ) {

		by <- c( "sic" )
		by.x <- by
		by.y <- by
		by <- c( "sic" )
		by.x.lk <- c( "edat", "H" )
		by.y.lk <- c( "edat", "height" )
		min <- c( -100, -10 )
		max <- c( +100, +10 )

		by.lk.x <-
			c(
				paste0(
					intersect(
						by.x.lk,
						by.y.lk ),
					".x" ),
				setdiff(
					by.y.lk,
					by.x.lk ) )

		by.lk.y <-
			c(
				paste0(
					intersect(
						by.x.lk,
						by.y.lk ),
					".y" ),
				setdiff(
					by.x.lk,
					by.y.lk ) )

		d <-
			merge(
				d1,
				d2,
				by.x = by.x,
				by.y = by.y )

		by.lk.x
		by.lk.y

		for( i in c( 1 : length( nms.id.x ) ) ) {
			if( is.Date( d1[ , nms.id.x[ i ] ] ) ) {
				cat( "date " )
				l <-  difftime( d1[ , nms.id.x[ i ] ], d2[ , nms.id.y[ i ] ], units = "day" )
				l.min.max <- ( min[ i ] < l & l <= max[ i ] )
				d <- d[ l.min.max, ]
				} else {
					if( is.numeric( d1[ 1, nms.id.x[ i ] ] ) == "numeric" ) {
						cat( "num " )
						l <- d1[ , nms.id.x[ i ] ] - d2[ , nms.id.y[ i ] ]
						l.min.max <- ( min[ i ] < l & l <= max[ i ] )
						d <- d[ l.min.max, ] } } }

		c( by.lk.x, by.lk.y )

		if( trim ) {
			d[ , by.lk.y[ i ] ] <-
				NULL
			names( d )[ names( d ) == by.lk.x[ i ] ] <-
				sub( "(\\w*).x", "\\1", by.lk.x[ i ], perl = T )
		}

		if( reorder.names && !trim ) {
			d.n <-
				unlist(
					mapply( list,
							by.lk.x,
							by.lk.y ) )
			d <-
				d[ , c( setdiff( names( d ), d.n ), d.n ) ]
		}
		d }

l <-
	ml(
		d1 = d1,
		d2 = d2,
		by = c( "sic" ),
		by.x.lk = c( "edat", "H" ),
		by.y.lk = c( "edat", "height" ),
		min = c( -1, -1 ),
		max = c( +1, +1 ),
		reorder.names = T,
		trim = T )

l
