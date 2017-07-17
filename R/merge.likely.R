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
#' @param by.lk.x vector of column names for the left hand side dataframe for likely merging via different column names
#' @param by.lk.y vector of column names for the right hand side dataframe for likely merging via different column names
#' @param min vector of minimum limits for likely merging
#' @param max vector of maximum limits for likely merging
#' @param all merge and add remaining columns from both dataframes, default is FALSE
#' @param all.x merge and add remaining columns from left dataframes, default is FALSE
#' @param all.y merge and add remaining columns from right dataframes, default is FALSE
#' @param reorder.names gives the opportunity to reorder columns that way the pairs participated on merging lay next to each other. Doen't use it with trim = T! It makes no sence., default is TRUE
#' @param trim if trim is TRUE it selects the left hand side column for final dataframe and deletes the right hand side columns, default is FALSE
#' @param add.diffs if add.diffs is TRUE differences of likely merged columns are added to resulting dataframe, default is FALSE
#'
#' @return merged data frame
#' @export merge.likely
#'
#' @examples
#' d1<-data.frame(w=sample(1:10,20,T),x=sample(1:10,20,T),y=sample(1:10,20,T),z=sample(c(1:10),20,T),id=1)
#' d2<-data.frame(w=sample(1:10,20,T),x=sample(1:10,20,T),Y=sample(1:10,20,T),C=sample(c(1:10),20,T),id=2)
#' merge.likely(d1=d1,d2=d2,by=c("x"),by.lk=c("w","id"),by.lk.x=c("y"),by.lk.y=c("Y"),min = c(-2,-2,-2),max=c(1,1,1),reorder.names = F,trim=F,add.diffs=F)
#' merge.likely(d1=d1,d2=d2,by=c("x"),by.lk=c("w","id"),by.lk.x=c("y"),by.lk.y=c("Y"),min = c(-2,-2,-2),max=c(1,1,1),reorder.names = T,trim=F,add.diffs=F)
#' merge.likely(d1=d1,d2=d2,by=c("x"),by.lk=c("w","id"),by.lk.x=c("y"),by.lk.y=c("Y"),min = c(-2,-2,-2),max=c(1,1,1),reorder.names = F,trim=T,add.diffs=F)
#' merge.likely(d1=d1,d2=d2,by=c("x"),by.lk=c("w","id"),by.lk.x=c("y"),by.lk.y=c("Y"),min = c(-2,-2,-2),max=c(1,1,1),reorder.names = T,trim=T,add.diffs=F)
#' merge.likely(d1=d1,d2=d2,by=c("x"),by.lk=c("w","id"),by.lk.x=c("y"),by.lk.y=c("Y"),min = c(-2,-2,-2),max=c(1,1,1),reorder.names = F,trim=F,add.diffs=T)
#' merge.likely(d1=d1,d2=d2,by=c("x"),by.lk=c("w","id"),by.lk.x=c("y"),by.lk.y=c("Y"),min = c(-2,-2,-2),max=c(1,1,1),reorder.names = T,trim=F,add.diffs=T)
#' merge.likely(d1=d1,d2=d2,by=c("x"),by.lk=c("w","id"),by.lk.x=c("y"),by.lk.y=c("Y"),min = c(-2,-2,-2),max=c(1,1,1),reorder.names = F,trim=T,add.diffs=T)
#' merge.likely(d1=d1,d2=d2,by=c("x"),by.lk=c("w","id"),by.lk.x=c("y"),by.lk.y=c("Y"),min = c(-2,-2,-2),max=c(1,1,1),reorder.names = T,trim=T,add.diffs=T)
merge.likely <-
	function(
		d1,
		d2,
		by = NULL,
		by.x = NULL,
		by.y = NULL,
		by.lk = NULL,
		by.lk.x = NULL,
		by.lk.y = NULL,
		min = NULL,
		max = NULL,
		all = F,
		all.x = F,
		all.y = F,
		reorder.names = T,
		trim = F,
		add.diffs = F ) {

		# d1 = d.1
		# d2 = d.2
		# by = c( "x" )
		# by.x = c( "y", "z" )
		# by.y = c( "Y", "C" )
		# by.lk = c( "x" )
		# by.lk.x = c( "y", "z" )
		# by.lk.y = c( "Y", "Z" )
		# min = c( -4, -4, -4 )
		# max = c( +3, +3, +3 )

		by.x.tot <-
			c( by, by.x )

		by.y.tot <-
			c( by, by.y )

		by.lk.x.tot <-
			c( by.lk, by.lk.x )

		by.lk.y.tot <-
			c( by.lk, by.lk.y )

		by.lk.xy.equal.names <-
			intersect(
				by.lk.x.tot,
				by.lk.y.tot )

		if( 0 < length( by.lk.xy.equal.names ) ) {

			by.lk.x.eql.nms <-
				paste0(
					by.lk.xy.equal.names,
					".x" )

			by.lk.y.eql.nms <-
				paste0(
					by.lk.xy.equal.names,
					".y" )

			by.lk.x.tot. <-
				c(
					by.lk.x.eql.nms,
					setdiff( by.lk.x.tot, by.lk.y.tot )	)

			by.lk.y.tot <-
				c(
					by.lk.y.eql.nms,
					setdiff( by.lk.y.tot, by.lk.x.tot )	)

			by.lk.x.tot <-
				by.lk.x.tot. }

		d <-
			merge(
				d1,
				d2,
				by,
				by.x = by.x.tot,
				by.y = by.y.tot )

		for( i in seq_along( by.lk.x.tot ) ) {

			if( lubridate::is.Date( d[ 1, by.lk.x.tot[ i ] ] ) ) {

				l.1.Ihk37Z._dlk__NDu2786cmn <-
					difftime( d[ , by.lk.y.tot[ i ] ], d[ , by.lk.x.tot[ i ] ], units = "day" )

				l.min.max <-
					( min[ i ] < l.1.Ihk37Z._dlk__NDu2786cmn & l.1.Ihk37Z._dlk__NDu2786cmn <= max[ i ] )

				if( add.diffs ) {

					d <-
						cbind( d, l.1.Ihk37Z._dlk__NDu2786cmn )

					names( d )[ names( d ) == "l.1.Ihk37Z._dlk__NDu2786cmn" ] <-
						paste0( by.lk.y.tot[ i ], "-" , by.lk.x.tot[ i ] ) }

				d <-
					d[ l.min.max, ]

			} else {

				if( is.numeric( d[ 1, by.lk.x.tot[ i ] ] ) ) {

					l.1.Ihk37Z._dlk__NDu2786cmn <-
						d[ , by.lk.y.tot[ i ] ] - d[ , by.lk.x.tot[ i ] ]

					l.min.max <-
						( min[ i ] < l.1.Ihk37Z._dlk__NDu2786cmn & l.1.Ihk37Z._dlk__NDu2786cmn <= max[ i ] )

					if( add.diffs ) {
						d <-
							cbind( d, l.1.Ihk37Z._dlk__NDu2786cmn )

						names( d )[ names( d ) == "l.1.Ihk37Z._dlk__NDu2786cmn" ] <-
							paste0( by.lk.y.tot[ i ], "-" , by.lk.x.tot[ i ] ) }

					d <-
						d[ l.min.max, ] } }

			if( trim ) {

				d[ , by.lk.y.tot[ i ] ] <-
					NULL

				names( d )[ names( d ) == by.lk.x.tot[ i ] ] <-
					sub( "(\\w*).x", "\\1", by.lk.x.tot[ i ], perl = T ) } }

		if( reorder.names ) {

			if( !trim ) {

				if( add.diffs ) {

					d.n <-
						sapply( t( cbind( cbind( by.lk.x.tot, by.lk.y.tot ), paste0( by.lk.y.tot, "-", by.lk.x.tot ) ) ), paste )

				} else {

					by.lk.tot <-
						cbind( by.lk.x.tot, by.lk.y.tot )

					if( !is.null( by.lk.tot ) ) {

						d.n <-
							t( by.lk.tot ) }
					else {
						d.n <-
							"" }

					d <-
						d[ , c( setdiff( names( d ), d.n ), d.n ) ] }

			} else {

				if( add.diffs ) {

					d.n <-
						sapply( t( cbind( sub( "(\\w*).x", "\\1", by.lk.x.tot, perl = T ), paste0( by.lk.y.tot, "-", by.lk.x.tot ) ) ), paste )
				} else {

					d.n <-
						sub( "(\\w*).x", "\\1", by.lk.x.tot, perl = T )

					d <-
						d[ , c( setdiff( names( d ), d.n ), d.n ) ] } } }

		na.omit( d ) }
