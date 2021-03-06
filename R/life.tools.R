#' ADJUST LINEARLY
#'
#' @name adjust.linearly
#'
#' @description Adjust a dependent variable y linearly to several independent variables x1, x2 ...
#' @param formula A formula that gives the relationship of the adjustment.
#' @param data A dataframe which contains the dependent and the independent variables.
#'
#' @return A dataframe containing a new column with the adjusted values.
#' @export
#'
#' @examples
#' load.pkgs( c( "ggplot2" ) )
#' set.seed( 1 )
#' num <- 100
#' sexes <- c( "female", "male" )
#' d <- data.frame( sex = sample( sexes, num, T, prob = c( .3, .7 )  ), age = round( runif( num, 0, 20 ), 1 ) )
#' d$y <- c( 30, 35 )[ match( d$sex, sexes ) ] + c( 2, 3 )[ match( d$sex, sexes ) ] * d$age + rnorm( num, 0, 10 )
#' d <- adjust.linearly( y ~ age, d )
#' d <- adjust.linearly( y ~ sex, d )
#' d <- adjust.linearly( y ~ age * sex, d )
#' thm          <- list( theme_bw( ), geom_point( ), geom_smooth( method = "lm" ) )
#' thm.brown    <- list( theme_bw( ), geom_point( col = "green3" ), geom_smooth( col = "green3", method = "lm" ) )
#' thm.bicol    <- list.append( thm, scale_color_manual( values = c( "firebrick1", "steelblue3" ), guide = F ) )
#' thm.bicolfac <- list.append( thm.bicol, facet_grid( sex ~ . ) )
#' ggsubplot(
#'   ggplot( d, aes( age, y ) ) + thm.brown,
#'   ggplot( d, aes( age, y, col = sex ) ) + thm.bicol,
#'   ggplot( d, aes( age, y.adj.for.age ) ) + thm.brown,
#'   ggplot( d, aes( age, y.adj.for.age, col = sex ) ) + thm.bicol,
#'   ggplot( d, aes( age, y.adj.for.sex ) ) + thm.brown,
#'   ggplot( d, aes( age, y.adj.for.sex, col = sex ) ) + thm.bicol,
#'   ggplot( d, aes( age, y.adj.for.age.sex, col = sex ) ) + thm.bicol,
#'   ggplot( d, aes( age, y.adj.for.age.sex, col = sex ) ) + thm.bicolfac,
#'   cols = 4 )
adjust.linearly <-
	function( formula, data ) {

		a.v <-
			all.vars( formula )

		nm <-
			paste0( a.v[ 1 ], ".adj.for" )

		for( a in a.v[ -1 ] )

			nm <-
				paste0( nm, ".", a )

		data[ , nm ] <-
			( lm( formula, data ) )$residuals + mean( unlist( data[ , a.v[ 1 ] ] ), na.rm = T )

		data
	}

########################################################################
#' ADJUST LINEARLY and STANDARIZE
#'
#' @name adjust.linearly.std
#'
#' @description Adjust a dependent variable y linearly to several independent variables x1, x2 ...
#' and standardize afterwards the adjusted values.
#' Requirements to the data: Homoscedasticity and Linearity of y ~ x...
#' @param formula A formula that gives the relationship of the adjustment.
#' @param data A dataframe which contains the dependent and the independent variables.
#'
#' @return A dataframe containing a new column with the adjusted and standardized values.
#' @export
#'
#' @examples
#' hlpr4life::load.pkgs( c( "hlpr4life", "ggplot2" ) )
#' set.seed( 1 )
#' num <- 100
#' sexes <- c( "female", "male" )
#' d <- data.frame( sex = sample( sexes, num, T, prob = c( .3, .7 )  ), age = round( runif( num, 0, 20 ), 1 ) )
#' d$y <- c( 30, 35 )[ match( d$sex, sexes ) ] + c( 2, 3 )[ match( d$sex, sexes ) ] * d$age + rnorm( num, 0, 10 )
#' d <- adjust.linearly.std( y ~ age, d )
#' d <- adjust.linearly.std( y ~ sex, d )
#' d <- adjust.linearly.std( y ~ age * sex, d )
#' thm          <- list( theme_bw( ), geom_point( ), geom_smooth( method = "lm" ) )
#' thm.brown    <- list( theme_bw( ), geom_point( col = "green3" ), geom_smooth( col = "green3", method = "lm" ) )
#' thm.bicol    <- list.append( thm, scale_color_manual( values = c( "firebrick1", "steelblue3" ), guide = F ) )
#' thm.bicolfac <- list.append( thm.bicol, facet_grid( sex ~ . ) )
#' ggsubplot(
#'   ggplot( d, aes( age, y ) ) + thm.brown,
#'   ggplot( d, aes( age, y, col = sex ) ) + thm.bicol,
#'   ggplot( d, aes( age, y.std.for.age ) ) + thm.brown,
#'   ggplot( d, aes( age, y.std.for.age, col = sex ) ) + thm.bicol,
#'   ggplot( d, aes( age, y.std.for.sex ) ) + thm.brown,
#'   ggplot( d, aes( age, y.std.for.sex, col = sex ) ) + thm.bicol,
#'   ggplot( d, aes( age, y.std.for.age.sex, col = sex ) ) + thm.bicol,
#'   ggplot( d, aes( age, y.std.for.age.sex, col = sex ) ) + thm.bicolfac,
#'   cols = 4 )
adjust.linearly.std <-
	function( formula, data ) {

		a.v <-
			all.vars( formula )

		nm <-
			paste0( a.v[ 1 ], ".std.for" )

		for( a in a.v[ -1 ] )

			nm <-
				paste0( nm, ".", a )

		data[ , nm ] <-
			( lm( formula, data ) )$residuals

		data[ , nm ] <-
			data[ , nm ] / sd( unlist( data[ , nm ] ), na.rm = T )

		data
	}

#' CALENDAR
#'
#' @description creates a simple calendar from first date to last date
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

		d
	}

#' GET COLUMNS
#' @name get.columns
#'
#' @description get.columns searches for column names that matches a given the pattern.
#' @param data dataframe which has some date columns
#' @param pattern search pattern for finding column names via grep
#'
#' @return names of searched columns
#' @export
#'
#' @examples
#' (d<-data.frame(SIC="LI01234",Y=1000.,x=10,SCI_GROUP="A2_12",DATE="2017-10-05",EDAT="2017-10-04"))
#' get.columns(d)
get.columns <-
	function( data, pattern = "DAT|SIC|GROUP", perl = T ) {

		names( data )[ grep( tolower( pattern ), tolower( names( data ) ), perl = T ) ]
	}

#' GET DATE COLUMNS
#' @name get.date.columns
#'
#' @description searches for some date like column names. one has the opportunity to give a
#' search string for a certain pattern.
#' @param data dataframe which has some date columns
#' @param pattern search pattern for finding column names via grep
#' @param perl logical: Use perl regex. Default is T.
#'
#' @return names of date columns
#' @export
#'
#' @examples
#' (d<-data.frame(DATE="2017-10-05",EDAT="2017-10-04",dat="2017-10-03",DATA="2017-10-02"))
#' get.date.columns(d)
#' (d<-data.frame(DATE="2017-10-05",EDAT="2017-10-04",MUTAD="2017-10-03"))
#' get.date.columns(d,"dat|muta")
get.date.columns <-
	function( data, pattern = "edat|date|datum", perl = T ) {

		get.columns( data, pattern, perl )
	}

#' GET MERGING INFOS
#' @name get.merging.infos
#'
#' @description get merging infos returns some usefull information of a set of tables.
#' Could be usefull before a merging process.
#'
#' @param table.names vector of table names
#'
#' @return data frame with merging informations for all given tables
#' @export
#'
#' @examples
#' (a<-data.frame(SGROUP="A2_02",DATE="200-10-05",Sic="LI12345678"))
#' (b<-data.frame(GRUPPE="A3_02",DATUM="200-10-05",PSEUDONYM="LI12345678"))
#' (c<-data.frame(GRP="A2_02",DATE="2001-10-05",EDAT="2001-10-04",PSEUDO="LI12345679",edat.new="2017-10-03"))
#' (infos<-get.merging.infos(c("a","b","c")))
#' infos$SIC
#' infos$SCI_GROUP
#' infos$DATE
get.merging.infos <-
	function( table.names = ls( ) ) {

		l <-
			list( )

		for( n in table.names ) {

			tbl <-
				as.data.frame( mget( n, .GlobalEnv )[[ 1 ]] )

			l[[ length( l ) + 1 ]] <-
				list(
					NAME      = n,
					SIC       = list( get.sic.columns( tbl ) ),
					SCI_GROUP = list( get.scigroup.columns( tbl ) ),
					DATE      = list( get.date.columns( tbl ) ),
					VISITS    = nrow( tbl ),
					VISITORS  = length( unique( tbl[ , ( get.sic.columns( tbl ) )[ 1 ] ] ) ),
					COMPLETE  = sum( complete.cases( tbl ) ) )
		}

		Reduce( dplyr::bind_rows, l )
	}

#' GET SCI-GROUP COLUMNS
#' @name get.scigroup.columns
#'
#' @description get.scigroup searches for some sci-group like column names. One has the opportunity to give a search string for a certain pattern.
#'
#' @param data dataframe which has some sci-group columns
#' @param pattern search pattern for finding column names via grep
#' @param perl logical: Use perl regex. Default is T.
#'
#' @return names of sci-group columns
#' @export
#'
#' @examples
#' (d<-data.frame(SGROUP="A2_02",SCI_GROUP="B1_10",Gruppe="A1-SK_10",GRP="A3_09"))
#' get.scigroup.columns(d)
get.scigroup.columns <-
	function( data, pattern = "sci_group|sci-group|scigroup|sgroup|group|grp|gruppe", perl = T ) {

		get.columns( data, pattern, perl )
	}

#' GET SIC COLUMNS
#' @name get.sic.columns
#'
#' @description searches for some sic like column names. one has the opportunity to give a
#' search string for a certain pattern.
#'
#' @param data dataframe which has some sic columns
#' @param pattern search pattern for finding column names via grep
#' @param perl logical: Use perl regex. Default is T.
#'
#' @return names of sic columns
#' @export
#'
#' @examples
#' (d<-data.frame(SIC="LI12345678",sic="LI12345679",PSEUDO="LI1234567X",PSEUDONYM="LI12345670"))
#' get.sic.columns(d)
get.sic.columns <-
	function( data, pattern = "sic|pseudo", perl = T ) {

		get.columns( data, pattern, perl )
	}

#' IF NOT
#'
#' @description short for if( !... ) do something else do something different
#' @param cond condition that hast to be false
#' @param optTrue option running if condition is not TRUE
#' @param optFalse option running if condition is not FALSE
#'
#' @return execution of code either for optTrue or optFalse
#' @export
#'
#' @examples
#' ifnot( 1 == 2, cat( "one is not equal to two" ), cat( "one is not not equal to two" ) )
ifnot <-
	function( cond, optTrue, optFalse = { } ) {

		if( !cond ) {

			optTrue

		} else {

			optFalse
		}
	}

#' LOAD PACKAGES
#' @name load.pkgs
#'
#' @description load.pkgs tries to load a package into the environment.
#' If this fails, it tries to install the package from CRAN-mirror and loads it afterwards.
#' @param pkgs
#'
#' @export
#'
#' @examples
#' hlpr4life::load.pkgs(c("hlpr4life","dplyr","ggplot2","ggthemes","reshape2"))
load.pkgs <-
	function( pkgs = c( "hlpr4life" ) ) {

		if( "hlpr4life" %in% pkgs ) {

			if( !"devtools" %in% rownames( installed.packages( ) ) ) {

				install.packages( "devtools" )
			}

			devtools::install_github( "TPeschel/hlpr4life" )

			library( "hlpr4life" )
		}

		exist <-
			pkgs %in% rownames( installed.packages( ) )

		if( any( !exist ) )

			install.packages( pkgs[ !exist ] )

		sapply( pkgs, library, character.only = TRUE )
	}

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
#' ( lst <- list.append( lst, x = Sys.time( ), name = "TIME" ) )
#' ( lst <- list.append( lst, "y", "Ypsilon" ) )
#' ( lst <- list.append( lst, "unnamed" ) )
list.append <-
	function( list, x, name = NA ) {

		list[[ length( list ) + 1 ]] <-
			x

		if( !is.na( name ) )

			names( list )[ length( list ) ] <-
				name

		list
	}

#' NOW
#'
#' @description Gives current time
#'
#' @return the current time
#' @export
#'
#' @examples
#' now()
now <-
	function( ) {

		substr( Sys.time( ), 12, 21)
	}

#' PRINT MERGING INFOS
#' @name print.merging.infos
#'
#' @description print merging infos print some usefull information of a set of tables.
#' Could be usefull before a merging process.
#'
#' @param table.names vector of table names
#'
#' @return prints a data frame with merging informations for all given tables
#' @export
#' @examples
#' (a<-data.frame(SGROUP="A2_02",DATE="2002-10-05",Sic="LI12345678"))
#' (b<-data.frame(GRUPPE="A3_02",DATUM="2002-10-05",PSEUDONYM="LI12345678"))
#' (c<-data.frame(GRP=c("A2_02","A2_03"),DATE=c("2002-10-05","2001-10-05"),EDAT=c("2001-10-04","2001-10-02"),PSEUDO=c("LI12345679","LI1234567X"),edat.new=c("2017.10-03","2017.10-01")))
#' print.merging.infos(c("a","b","c"))
print.merging.infos <-
	function( table.names = ls( ) ) {

		for( n in table.names ) {

			tbl <-
				as.data.frame( mget( n, .GlobalEnv )[[ 1 ]] )

			cat( c( "NAME:      ", n, "\n" ) )
			cat( c( "SIC:       ", get.sic.columns( tbl ), "\n" ) )
			cat( c( "SCI_GROUP: ", get.scigroup.columns( tbl ), "\n" ) )
			cat( c( "DATE:      ", get.date.columns( tbl ), "\n" ) )
			cat( c( "VISITS:    ", nrow( tbl ), "\n" ) )
			cat( c( "VISITORS:  ", length( unique( tbl[ , ( get.sic.columns( tbl ) )[ 1 ] ] ) ), "\n" ) )
			cat( c( "COMPLETE:  ", sum( complete.cases( tbl ) ), "\n" ) )
			cat( "_______________________________\n" )
		}
	}

#' REMOVE COLUMNS
#'
#' @description removes columns out of a dataframe
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

#' REMOVE LIST ELEMENTS
#'
#' @description removes elements out of a list
#' @param data A list that contains named elements which should be removed.
#' @param names Names of elements that should be removed.
#'
#' @return A list without removed columns.
#' @export
#'
#' @examples
#' remove.list.elements(list(x="X",y="Y",z="Z",w="W"),c("x","z"))
remove.list.elements <-
	function( list, names ) {

		list[ !names( list ) %in% names ]
	}

#' RENAME COLUMNS
#' @name rename.columns
#'
#' @description rename.columns takes a data frame d containing the old column names and returns a new dataframe with the replaced names.
#'
#' @param data dataframe with new column names
#' @param old.column.names old column names
#' @param new.column.names new column names
#'
#' @return dataframe with replaced column names
#' @export
#'
#' @examples
#' (d<-rename.columns(d<-data.frame(x=c(1:10),y=rnorm(10),z=c(10:1)),c("y","x"),c("x","y")))
#' (d<-rename.columns(d,c("y","x"),c("x","y")))
rename.columns <-
	function( data, old.column.names, new.column.names ) {

		n <-
			colnames( data )

		m <-
			n %in% old.column.names

		colnames( data )[ m ] <-
			new.column.names[ match( n[ m ], old.column.names ) ]

		data
	}

#' RENAME LIST
#'
#' RENAME LIST ELEMENTS
#' @name rename.list.elements
#'
#' @description rename.list.elements takes a list containing the old element names "alter" and "groesse" and returns a new list with the replaced names "age" and "size".
#'
#' @param list named list
#' @param old.names old names
#' @param new.names new names
#'
#' @return dataframe with replaced column names
#' @export
#'
#' @examples
#' (l<-rename.list.elements(list(x="x",y="y",z="z"),c("y","z","x"),c("Ypsilon","CED","U")))
rename.list.elements <-
	function( list, old.names, new.names ) {

		n <-
			names( list )

		m <-
			n %in% old.names

		names( list )[ m ] <-
			new.names[ match( n[ m ], old.names ) ]

		list
	}


#' SOME SICS
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

		paste0( prefix, stringr::str_pad( c( n.first : n.last ), digits, pad = "0" ), postfix )
	}

#' SUM AVAILABLES
#'
#' @name sum.av
#' @description gives the number of the available values of every column of a dataframe
#'
#' @param data dataframe for which availables in columns should be summed up
#'
#' @return dataframe with the number of available data for each column
#' @export
#'
#' @examples
#' (d<-data.frame(x=c(NA,"Hello",NA,"World",NA),y=c(1:5),z=rep(NA,5)))
#' sum.av(d)
sum.av <-
	function( data ) {

		sapply( data, function( col ) sum( !is.na( col ) ) )
	}

#' SUM MISSINGS
#' @name sum.na
#'
#' @description gives the number of the missing values of every column of a dataframe
#'
#' @param data dataframe for which missings in columns should be summed up
#'
#' @return dataframe with the number of missing data for each column
#' @export
#'
#' @examples
#' (d<-data.frame(x=c(NA,"Hello",NA,"World",NA),y=c(1:5),z=rep(NA,5)))
#' sum.na(d)
sum.na <-
	function( data ) {

		sapply( data, function( col ) sum( is.na( col ) ) )
	}

#' TABLE DATAFRAME
#' @name table.df
#'
#' @description table.df returns data about missings, availables of every column in a given
#' dataframe. If  summary is TRUE, min, max, median and mean are shown additionally.
#'
#' @param data A dataframe which columns should be summarised.
#' @param horizontal logical: The result is shown in horizontal or vertical style. Default is TRUE.
#' @param summary logical: If summary is TRUE additionally min, median, mean and max are tabled.
#' Default is FALSE.
#' @param na.rm logical: If na.rm is TRUE missings are ignored. Default is FALSE.
#' @param sort.by character: If sort.by is NAMES, then it is sorted by column names of the table.
#' One can also sort by the rownames as MISSING or MEAN if summary is TRUE. Default is "NO"
#'
#' @return A dataframe containing sum of missing and available data and if summary is TRUE mins
#' and maxs, means and medians of every column of a given datframe.
#' @export
#'
#' @examples
#' table.df(data.frame(x=c(1:3),y=c(NA,1,NA),z=c(NA,NA,NA),n=c("blonde","brown","black")))
#' table.df(data.frame(x=c(1:3),y=c(NA,1,NA),z=c(NA,NA,NA),n=c("blonde","brown","black")),F,F,F,"NO")
#' table.df(data.frame(x=c(1:3),y=c(NA,1,NA),z=c(NA,NA,NA),n=c("blonde","brown","black")),F,T,F,"MISSING")
#' table.df(data.frame(x=c(1:3),y=c(NA,1,NA),z=c(NA,NA,NA),n=c("blonde","brown","black")),T,F,F,"AVAILABLE")
#' table.df(data.frame(x=c(1:3),y=c(NA,1,NA),z=c(NA,NA,NA),n=c("blonde","brown","black")),T,T,F,"NAMES")
#' table.df(data.frame(x=c(1:3),y=c(NA,1,NA),z=c(NA,NA,NA),n=c("blonde","brown","black")),F,F,T,"MIN")
#' table.df(data.frame(x=c(1:3),y=c(NA,1,NA),z=c(NA,NA,NA),n=c("blonde","brown","black")),T,T,T,"MAX")
#' table.df(data.frame(x=c(1:3),y=c(NA,1,NA),z=c(NA,NA,NA),n=c("blonde","brown","black")),T,T,T,"MEAN")
#' table.df(data.frame(x=c(1:3),y=c(NA,1,NA),z=c(NA,NA,NA),n=c("blonde","brown","black")),F,T,T,"MEDIAN")
table.df <-
	function( data, horizontal = T, summary = F, na.rm = T, sort.by = "NO" ) {

		options( warn = -1 )

		MISSING <-
			sum.na( data )

		AVAILABLE <-
			sum.av( data )

		if( summary ) {

			CLASS <-
				paste0(
					sapply(
						data,
						class ) )

			TYPE <-
				sapply(
					data,
					typeof )

			MIN <-
				sapply(
					data,
					function( d )
						ifelse(
							!is.numeric( d ) && !is.logical( d ),
							min( as.character( d ), na.rm = T ),
							min( d, na.rm = na.rm ) ) )

			MAX <-
				sapply(
					data,
					function( d )
						ifelse(
							!is.numeric( d ) && !is.logical( d ),
							max( as.character( d ), na.rm = T ),
							max( d, na.rm = na.rm ) ) )

			MEAN <-
				sapply(
					data,
					function( d )
						ifelse(
							!is.numeric( d ) && !is.logical( d ),
							"not numeric",
							mean( d, na.rm = na.rm ) ) )

			MEDIAN <-
				sapply(
					data,
					function( d )
						ifelse(
							!is.numeric( d ) && !is.logical( d ),
							"not numeric",
							median( d, na.rm = na.rm ) ) )
		}

		d <-
			rbind(
				AVAILABLE,
				MISSING )

		if( summary ) {

			d <-
				rbind(
					d,
					CLASS,
					TYPE,
					MIN,
					MEDIAN,
					MEAN,
					MAX )
		}

		options( warn = 0 )

		d <-
			as.data.frame( d )

		if( any( sort.by == rownames( d ) ) ) {

			d <-
				d[ , order( d[ rownames( d ) == sort.by, ] ) ]
		}

		if( any( sort.by == "NAMES" ) ) {

			d <-
				d[ , order( colnames( d ) ) ]
		}

		d <-
			as.data.frame( d )

		if( horizontal ) {

			return( d )
		}

		return( as.data.frame( t( d ) ) )
	}

#' TODAY
#'
#' @description Gives the current date.
#'
#' @return the current date
#' @export
#'
#' @examples
#' today()
today <-
	function( ) {

		lubridate::date( Sys.time( ) )
	}

#' KEY.COLS
#'
#' @description key.cols creates a key out of several columns
#' @param col.x lhs column name
#' @param col.y rhs column name
#' @param sep a separator for binding column contents
#'
#' @return a key combined out of given columns
#' @export
#'
#' @examples
#' key.col( letters[3:1], LETTERS[1:3],"~")
key.col <-
	function( col.x, col.y, sep = "~" ) {

		paste0( col.x, sep, col.y ) }


#' KEY.DF
#'
#' @description key.df creates a key out of several columns of a dataframe
#' @param data a dataframe
#' @param column.names column names that should be used for creating a unique key
#' @param sep a separator for binding column contents
#' key.df(data.frame(x=letters[ runif(10,1,10)],y=LETTERS[runif(10,1,10)],z=rnorm(10)),c("x","y"),"~")
key.df <-
	function( data, column.names, sep = "~" ) {
		cl <-
			data[[ column.names[ 1 ] ]]

		for( i in 2 : length( column.names ) ) {

			cl <-
				paste0( cl, sep, data[[ column.names[ i ] ]] )
		}

		cl
	}
