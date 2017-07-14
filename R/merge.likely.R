#' merge likely
#'
#' @description merge likely merges two date frames by exact matching also by nearly matching. If the difference of two variable A of dataframe 1 and variable B of dataframe 2 may lay in an interval, one has the opportunity to give the limits of the interval by min and max.
#'
#' @param d1 left hand side dataframe
#' @param d2 right hand side dataframe
#' @param by vector of column names for exact merging via equal column names
#' @param by.x vector of column names for the left hand side dataframe for exact merging via different column names
#' @param by.y vector of column names for the right hand side dataframe for exact merging via different column names
#' @param by.lk vector of column names for likely merging via equal column names
#' @param by.x.lk vector of column names for the left hand side dataframe for likely merging via different column names
#' @param by.y.lk vector of column names for the right hand side dataframe for likely merging via different column names
#' @param min vector of minimum limits for likely merging
#' @param max vector of maximum limits for likely merging
#' @param reorder.names gives the opportunity to reorder columns that way the pairs participated on merging lay next to each other. Doen't use it with trim = T! It makes no sence.
#' @param trim if trim is TRUE it selects the left hand side column for final dataframe and deletes the right hand side columns
#'
#' @return merged data frame
#' @export merge.likely
#'
#' @examples
#' d1<-data.frame(w=sample(1:10,20,T),x=sample(1:10,20,T),y=sample(1:10,20,T),z=sample(c(1:10),20,T),id=1)
#' d2<-data.frame(w=sample(1:10,20,T),x=sample(1:10,20,T),Y=sample(1:10,20,T),C=sample(c(1:10),20,T),id=2)
#' ml(d1=d1,d2=d2,by=c("x"),by.lk=c("w","id"),by.x.lk=c("y"),by.y.lk=c("Y"),min = c(-2,-2,-2),max=c(1,1,1),reorder.names = F,trim = F,add.diffs=F)
#' ml(d1=d1,d2=d2,by=c("x"),by.lk=c("w","id"),by.x.lk=c("y"),by.y.lk=c("Y"),min = c(-2,-2,-2),max=c(1,1,1),reorder.names = T,trim = F,add.diffs=F)
#' ml(d1=d1,d2=d2,by=c("x"),by.lk=c("w","id"),by.x.lk=c("y"),by.y.lk=c("Y"),min = c(-2,-2,-2),max=c(1,1,1),reorder.names = F,trim = T,add.diffs=F)
#' ml(d1=d1,d2=d2,by=c("x"),by.lk=c("w","id"),by.x.lk=c("y"),by.y.lk=c("Y"),min = c(-2,-2,-2),max=c(1,1,1),reorder.names = T,trim = T,add.diffs=F)
#' ml(d1=d1,d2=d2,by=c("x"),by.lk=c("w","id"),by.x.lk=c("y"),by.y.lk=c("Y"),min = c(-2,-2,-2),max=c(1,1,1),reorder.names = F,trim = F,add.diffs=T)
#' ml(d1=d1,d2=d2,by=c("x"),by.lk=c("w","id"),by.x.lk=c("y"),by.y.lk=c("Y"),min = c(-2,-2,-2),max=c(1,1,1),reorder.names = T,trim = F,add.diffs=T)
#' ml(d1=d1,d2=d2,by=c("x"),by.lk=c("w","id"),by.x.lk=c("y"),by.y.lk=c("Y"),min = c(-2,-2,-2),max=c(1,1,1),reorder.names = F,trim = T,add.diffs=T)
#' ml(d1=d1,d2=d2,by=c("x"),by.lk=c("w","id"),by.x.lk=c("y"),by.y.lk=c("Y"),min = c(-2,-2,-2),max=c(1,1,1),reorder.names = T,trim = T,add.diffs=T)
merge.likely <-
	function(
		d1,
		d2,
		by = NULL,
		by.x = NULL,
		by.y = NULL,
		by.lk = NULL,
		by.x.lk = NULL,
		by.y.lk = NULL,
		min = NULL,
		max = NULL,
		reorder.names = T,
		trim = F,
		add.diffs = F ) {

		by.x <-
			c( by, by.x )

		by.y <-
			c( by, by.y )

		by.x.lk <-
			c( by.lk, by.x.lk )

		by.y.lk <-
			c( by.lk, by.y.lk )

		by.lk.xy.equal.names <-
			intersect(
				by.x.lk,
				by.y.lk )

		if( 0 < length( by.lk.xy.equal.names ) ) {

			by.lk.x <-
				paste0(
					by.lk.xy.equal.names,
					".x" )
			by.lk.y <-
				paste0(
					by.lk.xy.equal.names,
					".y" ) }
		by.lk.x <-
			c(
				by.lk.x,
				setdiff( by.x.lk, by.y.lk )	)

		by.lk.y <-
			c(
				by.lk.y,
				setdiff( by.y.lk, by.x.lk )	)

		d <-
			merge(
				d1,
				d2,
				by.x = by.x,
				by.y = by.y )

		for( i in seq_along( by.lk.x ) ) {

			if( is.Date( d[ 1, by.lk.x[ i ] ] ) ) {

				l.1.Ihk37Z._dlk__NDu2786cmn <-
					difftime( d[ , by.lk.y[ i ] ], d[ , by.lk.x[ i ] ], units = "day" )

				l.min.max <-
					( min[ i ] < l.1.Ihk37Z._dlk__NDu2786cmn & l.1.Ihk37Z._dlk__NDu2786cmn <= max[ i ] )

				if( add.diffs ) {

					d <-
						cbind( d, l.1.Ihk37Z._dlk__NDu2786cmn )

					names( d )[ names( d ) == "l.1.Ihk37Z._dlk__NDu2786cmn" ] <-
						paste0( by.lk.y[ i ], "-" , by.lk.x[ i ] )
				}

				d <-
					d[ l.min.max, ]

				} else {

					if( is.numeric( d[ 1, by.lk.x[ i ] ] ) ) {

						l.1.Ihk37Z._dlk__NDu2786cmn <-
							d[ , by.lk.y[ i ] ] - d[ , by.lk.x[ i ] ]

						l.min.max <-
							( min[ i ] < l.1.Ihk37Z._dlk__NDu2786cmn & l.1.Ihk37Z._dlk__NDu2786cmn <= max[ i ] )

						if( add.diffs ) {
							d <-
								cbind( d, l.1.Ihk37Z._dlk__NDu2786cmn )

							names( d )[ names( d ) == "l.1.Ihk37Z._dlk__NDu2786cmn" ] <-
								paste0( by.lk.y[ i ], "-" , by.lk.x[ i ] )
						}

						d <-
							d[ l.min.max, ]

					} }

			if( trim ) {

				d[ , by.lk.y[ i ] ] <-
					NULL

				names( d )[ names( d ) == by.lk.x[ i ] ] <-
					sub( "(\\w*).x", "\\1", by.lk.x[ i ], perl = T ) } }

		if( reorder.names ) {

			if( !trim ) {

				if( add.diffs )

					d.n <-
						sapply( t( cbind( cbind( by.lk.x, by.lk.y ), paste0( by.lk.y, "-", by.lk.x ) ) ), paste )
				else

					d.n <-
						t( cbind( by.lk.x, by.lk.y ) )

				d <-
					d[ , c( setdiff( names( d ), d.n ), d.n ) ]

			} else {

				if( add.diffs )

					d.n <-
						sapply( t( cbind( sub( "(\\w*).x", "\\1", by.lk.x, perl = T ), paste0( by.lk.y, "-", by.lk.x ) ) ), paste )
				else

					d.n <-
						sub( "(\\w*).x", "\\1", by.lk.x, perl = T )

				d <-
					d[ , c( setdiff( names( d ), d.n ), d.n ) ]
			}
		}

		na.omit( d ) }
