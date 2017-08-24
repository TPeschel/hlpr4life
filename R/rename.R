#' rename column
#' @description rename.column( d, "old", "new" ) changes the old column name "old" to the new one "new".
#'
#' @param d dataframe for which one name should be changed
#' @param old.column.name old column name
#' @param new.column.name new column name
#'
#' @return dataframe with new column name
#' @export
#'
#' @examples
#' (d<-rename.column(data.frame(x=c(1:10),y=rnorm(10)),"x","X"))
#' (d<-rename.column(d,"y","Y"))
rename.column <-
	function( d, old.column.name, new.column.name ) {

		names( d )[ names( d ) == old.column.name ] <-
			new.column.name

		d }

#' rename columns
#' @description rename.columns( d, c( "alter", "groesse" ), c( "age", "height" ) ) takes a data frame d containing the old column name "alter" and "groesse" and returns a new dataframe with the replaced names "age" and "size".
#'
#' @param d dataframe with new column names
#' @param old.column.names old column names
#' @param new.column.names new column names
#'
#' @return dataframe with replaced column names
#' @export
#'
#' @examples
#' (d<-rename.columns(data.frame(x=c(1:10),y=rnorm(10),z=c(10:1)),c("y","x"),c("x","y")))
#' (d<-rename.columns(d,c("y","x"),c("x","y")))
rename.columns <-
	function( d, old.column.names, new.column.names ) {

		n <-
			colnames( d )

		m <-
			n %in% old.column.names

		colnames( d )[ m ] <-
			new.column.names[ match( n[ m ], old.column.names ) ]

		d }
