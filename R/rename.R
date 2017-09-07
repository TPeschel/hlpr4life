#' RENAME COLUMN
#' @description rename.column( data, "old", "new" ) changes the old column name "old" to the new one "new".
#'
#' @param data dataframe for which one name should be changed
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
	function( data, old.column.name, new.column.name ) {

		names( data )[ names( data ) == old.column.name ] <-
			new.column.name

		data }

#' RENAME COLUMNS
#' @description rename.columns( data, c( "alter", "groesse" ), c( "age", "height" ) ) takes a data frame d containing the old column name "alter" and "groesse" and returns a new dataframe with the replaced names "age" and "size".
#'
#' @param data dataframe with new column names
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
	function( data, old.column.names, new.column.names ) {

		n <-
			colnames( data )

		m <-
			n %in% old.column.names

		colnames( data )[ m ] <-
			new.column.names[ match( n[ m ], old.column.names ) ]

		data }

#' RENAME LIST
#' @description rename.columns( data, c( "alter", "groesse" ), c( "age", "height" ) ) takes a data frame d containing the old column name "alter" and "groesse" and returns a new dataframe with the replaced names "age" and "size".
#'
#' @param list named list
#' @param old.names old names
#' @param new.names new names
#'
#' @return dataframe with replaced column names
#' @export
#'
#' @examples
#' (l<-rename.list(list(x="x",y="y",z="z"),c("y","z","x"),c("Ypsilon","CED","U")))
rename.list <-
	function( list, old.names, new.names ) {

		n <-
			names( list )

		m <-
			n %in% old.names

		names( list )[ m ] <-
			new.names[ match( n[ m ], old.names ) ]

		list }
