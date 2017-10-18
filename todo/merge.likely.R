#' MERGE APPROXIMATELY
#'
#' @description merge.approx merges two date frames by exact matching also by nearly matching. If the difference of two variable A of dataframe 1 and variable B of dataframe 2 may lay in an interval, one has the opportunity to give the limits of the interval by min and max.
#'
#' @param d1 left hand side dataframe
#' @param d2 right hand side dataframe
#' @param by vector of column names for exact merging via equal column names
#' @param by.x vector of column names for the left hand side dataframe for exact merging via different column names
#' @param by.y vector of column names for the right hand side dataframe for exact merging via different column names
#' @param by.approx vector of column names for likely merging via equal column names
#' @param by.approx.x vector of column names for the left hand side dataframe for likely merging via different column names
#' @param by.approx.y vector of column names for the right hand side dataframe for likely merging via different column names
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
#' @export merge.approx
#'
#' @examples
#' (d1<-data.frame(w=sample(1:10,20,T),x=sample(1:10,20,T),y=sample(1:10,20,T),z=sample(c(1:10),20,T),id=1))
#' (d2<-data.frame(w=sample(1:10,20,T),x=sample(1:10,20,T),Y=sample(1:10,20,T),C=sample(c(1:10),20,T),id=2))
#' merge.approx(d1=d1,d2=d2,by=c("x"),by.approx=c("w","id"),by.approx.x=c("y"),by.approx.y=c("Y"),min=c(-2,-2,-2),max=c(1,1,1),reorder.names=F,trim=F,add.diffs=F)
#' merge.approx(d1=d1,d2=d2,by=c("x"),by.approx=c("w","id"),by.approx.x=c("y"),by.approx.y=c("Y"),min=c(-2,-2,-2),max=c(1,1,1),reorder.names=T,trim=F,add.diffs=F)
#' merge.approx(d1=d1,d2=d2,by=c("x"),by.approx=c("w","id"),by.approx.x=c("y"),by.approx.y=c("Y"),min=c(-2,-2,-2),max=c(1,1,1),reorder.names=F,trim=T,add.diffs=F)
#' merge.approx(d1=d1,d2=d2,by=c("x"),by.approx=c("w","id"),by.approx.x=c("y"),by.approx.y=c("Y"),min=c(-2,-2,-2),max=c(1,1,1),reorder.names=T,trim=T,add.diffs=F)
#' merge.approx(d1=d1,d2=d2,by=c("x"),by.approx=c("w","id"),by.approx.x=c("y"),by.approx.y=c("Y"),min=c(-2,-2,-2),max=c(1,1,1),reorder.names=F,trim=F,add.diffs=T)
#' merge.approx(d1=d1,d2=d2,by=c("x"),by.approx=c("w","id"),by.approx.x=c("y"),by.approx.y=c("Y"),min=c(-2,-2,-2),max=c(1,1,1),reorder.names=T,trim=F,add.diffs=T)
#' merge.approx(d1=d1,d2=d2,by=c("x"),by.approx=c("w","id"),by.approx.x=c("y"),by.approx.y=c("Y"),min=c(-2,-2,-2),max=c(1,1,1),reorder.names=F,trim=T,add.diffs=T)
#' merge.approx(d1=d1,d2=d2,by=c("x"),by.approx=c("w","id"),by.approx.x=c("y"),by.approx.y=c("Y"),min=c(-2,-2,-2),max=c(1,1,1),reorder.names=T,trim=T,add.diffs=T)
merge.approx <-
    function(
        x,
        y,
        by = NULL,
        by.x = NULL,
        by.y = NULL,
        by.approx = NULL,
        by.x.approx = NULL,
        by.y.approx = NULL,
        min = NULL,
        max = NULL,
        all = F,
        all.x = F,
        all.y = F,
        reorder.names = T,
        trim = F,
        add.diffs = F ) {

        by.x.tot <-
            union( by, setdiff( by.x, by ) )

        by.y.tot <-
        	union( by, setdiff( by.y, by ) )

        by.approx.x.tot <-
            union( by.approx, setdiff( by.x.approx, by.approx ) )

        by.approx.y.tot <-
        	union( by.approx, setdiff( by.y.approx, by.approx ) )

        by.approx.xy.equal.names <-
            intersect(
                by.approx.x.tot,
                by.approx.y.tot )

        if( 0 < length( by.approx.xy.equal.names ) ) {

            by.approx.x.eql.nms <-
                paste0(
                    by.approx.xy.equal.names,
                    ".x" )

            by.approx.y.eql.nms <-
                paste0(
                    by.approx.xy.equal.names,
                    ".y" )

            by.approx.x.tot. <-
                c(
                    by.approx.x.eql.nms,
                    setdiff( by.approx.x.tot, by.approx.y.tot )	)

            by.approx.y.tot <-
                c(
                    by.approx.y.eql.nms,
                    setdiff( by.approx.y.tot, by.approx.x.tot )	)

            by.approx.x.tot <-
                by.approx.x.tot. }

        d <-
            merge(
                d1,
                d2,
                by.x = by.x.tot,
                by.y = by.y.tot )

        if( all == T ) {
        	all.x <- T
        	all.y <- T
        }

        if( all.x == T ) {
        	d.allx <-
        		dplyr::anti_join(
	        		d1,
	        		d2,
	        		by.x = by.x.tot,
	        		by.y = by.y.tot ) }

        if( all.y == T ) {
        	d.ally <-
        		dplyr::anti_join(
	        		d2,
	        		d1,
	        		by.y = by.y.tot,
	        		by.x = by.x.tot ) }

        for( i in seq_along( by.approx.x.tot ) ) {

            if( lubridate::is.POSIXct( d[ 1, by.approx.x.tot[ i ] ] ) ) {

                l.1.Ihk37Z._dlk__NDu2786cmn <-
                    difftime( d[ , by.approx.y.tot[ i ] ], d[ , by.approx.x.tot[ i ] ], units = "day" )

                l.min.max <-
                    ( min[ i ] < l.1.Ihk37Z._dlk__NDu2786cmn & l.1.Ihk37Z._dlk__NDu2786cmn <= max[ i ] )

                if( add.diffs ) {

                    d <-
                        cbind( d, l.1.Ihk37Z._dlk__NDu2786cmn )

                    names( d )[ names( d ) == "l.1.Ihk37Z._dlk__NDu2786cmn" ] <-
                        paste0( by.approx.y.tot[ i ], "-" , by.approx.x.tot[ i ] ) }

                d <-
                    d[ l.min.max, ]

            } else {

                if( is.numeric( d[ 1, by.approx.x.tot[ i ] ] ) ) {

                    l.1.Ihk37Z._dlk__NDu2786cmn <-
                        d[ , by.approx.y.tot[ i ] ] - d[ , by.approx.x.tot[ i ] ]

                    l.min.max <-
                        ( min[ i ] < l.1.Ihk37Z._dlk__NDu2786cmn & l.1.Ihk37Z._dlk__NDu2786cmn <= max[ i ] )

                    if( add.diffs ) {
                        d <-
                            cbind( d, l.1.Ihk37Z._dlk__NDu2786cmn )

                        names( d )[ names( d ) == "l.1.Ihk37Z._dlk__NDu2786cmn" ] <-
                            paste0( by.approx.y.tot[ i ], "-" , by.approx.x.tot[ i ] ) }

                    d <-
                        d[ l.min.max, ] } }
        	if( all.x )
        		d <-
        			dplyr::bind_rows( d, d.allx )

        	if( all.y )
        		d <-
        			dplyr::bind_rows( d, d.ally )

        	if( trim ) {

                d[ , by.approx.y.tot[ i ] ] <-
                    NULL

                names( d )[ names( d ) == by.approx.x.tot[ i ] ] <-
                    sub( "(\\w*).x", "\\1", by.approx.x.tot[ i ], perl = T ) } }

        if( reorder.names ) {

            if( !trim ) {

                if( add.diffs ) {

                    d.n <-
                        sapply( t( cbind( cbind( by.approx.x.tot, by.approx.y.tot ), paste0( by.approx.y.tot, "-", by.approx.x.tot ) ) ), paste )

                } else {

                    by.approx.tot <-
                        cbind( by.approx.x.tot, by.approx.y.tot )

                    if( !is.null( by.approx.tot ) ) {

                        d.n <-
                            t( by.approx.tot ) }
                    else {
                        d.n <-
                            NULL }

                    d <-
                        d[ , c( setdiff( names( d ), d.n ), d.n ) ] }

            } else {

                if( add.diffs ) {

                    d.n <-
                        sapply( t( cbind( sub( "(\\w*).x", "\\1", by.approx.x.tot, perl = T ), paste0( by.approx.y.tot, "-", by.approx.x.tot ) ) ), paste )
                } else {

                    d.n <-
                        sub( "(\\w*).x", "\\1", by.approx.x.tot, perl = T )

                    d <-
                        d[ , c( setdiff( names( d ), d.n ), d.n ) ] } } }

        d }
