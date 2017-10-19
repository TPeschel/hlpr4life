# HELPER FOR LIFE

This is a small collection of some R-stuff for every day use.
It is recommended to install this package first time via 
devtools::install_github( "TPeschel/hlpr4life" )
```R
devtools::install_github( "TPeschel/hlpr4life" )
```

Of course for that it is neccessary to have devtools installed.
So I recommend the following lines for an easy use of hlpr4life
## first installation
```R
if( !"devtools" %in% rownames( installed.packages( ) ) {
	install.packages( "devtools" ) )
	library( "devtools" )
}
devtools::install_github( "TPeschel/hlpr4life" )

```
## daily use
From now on hlpr4life is kept in the latest version
available on github. The dots are for some further packages
that should be loaded and installed before if they are not.
```R
hlpr4life::load.pkgs( c( "hlpr4life", ... ) )
```

## simple example
```R
hlpr4life::load.pkgs( 
	c(
		"hlpr4life",
		"dplyr",
		"magrittr",
		"stringr",
		"lubridate",
		"ggplot2" ) )

n <-
	1000

( clndr <-
	calendar( 
		"2017-01-01",
		"2017-12-31" ) )

( sexes <-
	c( "female", "male" ) )
	
( d <-
	data.frame(
		sic  = s<-sample( some.sics( n.first = 100, n.last = 300 ), n, T ),
		date = sample( clndr$date, n, T ) ) )

( d$sex <-
	sample( sexes, n, T )[ match( d$sic, unique( d$sic ) ) ] )

( d$birth <-
	sample( calendar( "2000-01-01", "2016-12-31" )$date, length( unique( d$sic ) ), T )[ match( d$sic, unique( d$sic ) ) ] )

( d$age <-
	round( as.numeric( difftime( as.Date( d$date ), as.Date( d$birth ), units = "day" ) ) / 325.25, 2 ) )

( d$sci_group <-
	paste0( "A8_", str_pad( floor( d$age ), 2, pad = "0" ) ) )

( d$height <-
	round(
		rnorm( n, c( 30, 35 )[ match( d$sex, sexes ) ], 2 + 1 * d$age ) +
		( exp( 20 ) - exp( 20 - ( d$age * c( 1.01, 1.03 )[ match( d$sex, sexes ) ] - 1 ) / 15 ) ) * 200 / exp( 20 ), 2 ) )
		
( d$weight <-
	round(
		rnorm( n, c( 4, 4.5 )[ match( d$sex, sexes ) ], 1 + c( .6, .7 )[ match( d$sex, sexes ) ] * d$age ) +
		( exp( 20 ) - exp( 20 - ( d$age * c( 1.01, 1.02 )[ match( d$sex, sexes ) ] - 1 ) / 18 ) ) * 100 / exp( 20 ), 2 ) )
		
( d$bmi <-
	round( 10000 * d$weight / d$height / d$height, 2 ) )

( d <-
	arrange(
		d[ , c( "sic", "sci_group", "sex", "birth", "age", "date", "height", "weight", "bmi" ) ],
		sic,
		sci_group ) )

d <-
	adjust.linearly.std( weight ~ sex * age, d )

d <-
	adjust.linearly.std( height ~ sex * age, d )

d <-
	adjust.linearly.std( bmi ~ sex * age, d )

thm <-
	list( 
	theme_bw( ), 
	geom_point( alpha = .3 ), 
	geom_smooth( method = "lm" ),
	scale_color_manual( values = c( "red", "blue" ) ) )
	
ggsubplot(
	ggplot( d, aes( age, height, col = sex ) ) + thm,
	ggplot( d, aes( age, weight, col = sex ) ) + thm,
	ggplot( d, aes( age, bmi, col = sex ) ) + thm,
	ggplot( d, aes( age, height.std.for.sex.age, col = sex ) ) + thm,
	ggplot( d, aes( age, weight.std.for.sex.age, col = sex ) ) + thm,
	ggplot( d, aes( age, bmi.std.for.sex.age, col = sex ) ) + thm,
	cols = 2 )
```


## ADJUSTMENTS
### ADJUST LINEARLY 
Adjust a dependent variable y linearly to several independent variables x1, x2 ...
```R
hlpr4life::load.pkgs( c( "hlpr4life", "ggplot2" ) )

set.seed( 1 )

num <- 100

sexes <- c( "female", "male" )

d <- data.frame( sex = sample( sexes, num, T, prob = c( .3, .7 )  ), age = round( runif( num, 0, 20 ), 1 ) )

d$y <- c( 30, 35 )[ match( d$sex, sexes ) ] + c( 2, 3 )[ match( d$sex, sexes ) ] * d$age + rnorm( num, 0, 10 )

d <- adjust.linearly( y ~ age, d )
d <- adjust.linearly( y ~ sex, d )
d <- adjust.linearly( y ~ age * sex, d )

thm          <- list( theme_bw( ), geom_point( ), geom_smooth( method = "lm" ) )
thm.brown    <- list( theme_bw( ), geom_point( col = "green3" ), geom_smooth( col = "green3", method = "lm" ) )
thm.bicol    <- list.append( thm, scale_color_manual( values = c( "firebrick1", "steelblue3" ), guide = F ) )
thm.bicolfac <- list.append( thm.bicol, facet_grid( sex ~ . ) )

ggsubplot(
	ggplot( d, aes( age, y ) ) + thm.brown,
	ggplot( d, aes( age, y, col = sex ) ) + thm.bicol,
	ggplot( d, aes( age, y.adj.for.age ) ) + thm.brown,
	ggplot( d, aes( age, y.adj.for.age, col = sex ) ) + thm.bicol,
	ggplot( d, aes( age, y.adj.for.sex ) ) + thm.brown,
	ggplot( d, aes( age, y.adj.for.sex, col = sex ) ) + thm.bicol,
	ggplot( d, aes( age, y.adj.for.age.sex, col = sex ) ) + thm.bicol,
	ggplot( d, aes( age, y.adj.for.age.sex, col = sex ) ) + thm.bicolfac,
	cols = 4 )
```

### ADJUST LINEARLY AND STANDARDIZE 
Adjust and standardize afterwards a dependent variable y linearly to several independent variables x1, x2 ...
```R
hlpr4life::load.pkgs( c( "hlpr4life", "ggplot2" ) )

set.seed( 1 )

num <- 100

sexes <- c( "female", "male" )

d <- data.frame( sex = sample( sexes, num, T, prob = c( .3, .7 )  ), age = round( runif( num, 0, 20 ), 1 ) )

d$y <- c( 30, 35 )[ match( d$sex, sexes ) ] + c( 2, 3 )[ match( d$sex, sexes ) ] * d$age + rnorm( num, 0, 10 )

d <- adjust.linearly.std( y ~ age, d )
d <- adjust.linearly.std( y ~ sex, d )
d <- adjust.linearly.std( y ~ age * sex, d )

thm          <- list( theme_bw( ), geom_point( ), geom_smooth( method = "lm" ) )
thm.brown    <- list( theme_bw( ), geom_point( col = "green3" ), geom_smooth( col = "green3", method = "lm" ) )
thm.bicol    <- list.append( thm, scale_color_manual( values = c( "firebrick1", "steelblue3" ), guide = F ) )
thm.bicolfac <- list.append( thm.bicol, facet_grid( sex ~ . ) )

ggsubplot(
	ggplot( d, aes( age, y ) ) + thm.brown,
	ggplot( d, aes( age, y, col = sex ) ) + thm.bicol,
	ggplot( d, aes( age, y.std.for.age ) ) + thm.brown,
	ggplot( d, aes( age, y.std.for.age, col = sex ) ) + thm.bicol,
	ggplot( d, aes( age, y.std.for.sex ) ) + thm.brown,
	ggplot( d, aes( age, y.std.for.sex, col = sex ) ) + thm.bicol,
	ggplot( d, aes( age, y.std.for.age.sex, col = sex ) ) + thm.bicol,
	ggplot( d, aes( age, y.std.for.age.sex, col = sex ) ) + thm.bicolfac,
	cols = 4 )
```

## PACKAGE LOADER
### LOAD PACKAGES 
Load packages as library() or require() do!
If some are not installed then install them.
```R
load.pkgs( c( "dplyr", "ggplot2", "ggthemes", "reshape2" ) )
```

## TIME AND DATE
### CALENDAR
Build a dataframe with calendar data!
```R
clndr <- 
	calendar( 
		start = "2017-01-01", 
		end = "2017-12-31", 
		abbreviate = F, 
		tz = "Europe/Berlin" )

( weekends.in.2017 <-
	clndr[ clndr$day_of_week %in% c( "Samstag", "Sonntag" ), ] )

( working.days.in.2017 <-
	clndr[ clndr$day.of.week %in% c( 2 : 6 ), ] )

( my.birthday <-
	clndr[ clndr$date == "2017-11-28", -2 ] )

( summer.2017 <-
	calendar(
		start = "2017-06-21",
		end   = "2017-09-21" ) )
```

## COLORS
### PLOT COLORS
Plot all in R defined colors!
```R
plot.colors( )
```

### COLOR GRADIENT
Build a dataframe with a color gradient between two distinct colors
partitioned by steps!
```R
color.gradient( "red", "green", 16 )
```

### PLOT COLOR GRADIENT
Plot a color gradient between two distinct colors
partitioned by steps!
```R
plot.color.gradient( "red", "green", 256, "horiz", F )
plot.color.gradient( "blue", "yellow", 16, "vert", T )
```

## THE PIANO
### FREQUENCY OF A PIANO KEY'S SOUND
What's the frequency heard by playing key 25 of a piano?
```R
freq.of.key( 25 )
```
### FREQUENCY OF A NOTE
What's the frequency of a certain note?
```R
freq.of.note( "A2" )
```
### KEY OF FREQUENCY
Which key has to be pressed for a certain frequency?
```R
key.of.freq( 110 )
```

### KEY OF NOTE
Which key plays a certain note?
```R
key.of.note( c( "C0", "D0", "E0", "F0", "G0", "A1", "B1", "C1" ) )
```

### NOTE OF FREQUENCY
Which frequency is represented by a certain note?
```R
note.of.freq( 110 )
```

### NOTE OF KEY
Which note has a certain piano key?
```R
note.of.key(key = c(4, 6, 8, 9, 11, 13, 15, 16))
```

### PIANO
Description of a piano
```R
piano(left.key = 4, right.key = 88)
```
```R
load.pkgs( c( "ggplot2", "ggthemes" ) )
piano( key.of.note( "C1" ), key.of.note( "C3" ) )
ggplot( piano( ) ) +
	geom_histogram( aes( note, -c( 1, .63 )[ match( color, c( "ivory", "ebony" ) ) ], fill = color ), stat = "identity" ) +
	geom_histogram( aes( note, .001 * frequency, alpha = note ), fill = "orange", stat = "identity" ) +
	scale_fill_manual( values = c( "#000000", "#f0f3f4" ), guide = F ) +
	geom_text( aes( note, label = note, col = color ), y = -.2, angle = 90 ) +
	annotate( geom = "text", x = 24.98, y = 3.02, xmin = 30, xmax = 60, ymin = 1, ymax = 5, label = "THE PIANO", col = "white", size = 20 ) +
	annotate( geom = "text", x = 25,    y = 3,    xmin = 30, xmax = 60, ymin = 1, ymax = 5, label = "THE PIANO", col = "black", size = 20 ) +
	scale_color_manual( values = c( "#f0f3f4", "#000000" ), guide = F ) +
	scale_alpha_discrete( range = c(.7,.1),guide = F ) +
	theme_solid( fill = "#405060" ) +
	theme(
		axis.text.x = element_blank( ),
		axis.title.x = element_blank( ),
		axis.text.y = element_blank( ),
		axis.title.y = element_blank( ),
		legend.key = element_blank( ) )
```

## RENAMER
### RENAME COLUMNS
Rename some columns of a dataframe.
```R
(d<-rename.columns( data.frame(x=c(1:10),y=rnorm(10),z=c(10:1)),c("y","x"),c("x","y")))
```

Rename some elements of a list.
### RENAME LIST ELEMENTS
```R
(l<-rename.list.elements(list(x="x",y="y",z="z"),c("y","z","x"),c("Ypsilon","CED","U")))
```

Remove columns of a list.
### REMOVE LIST ELEMENTS
```R
remove.columns(data.frame(x="X",y="Y",z="Z",w="W"),c("x","z"))
```

Remove elements of a list.
### REMOVE LIST ELEMENTS
```R
remove.list.elements(list(x="X",y="Y",z="Z",w="W"),c("x","z"))
```

## LIST APPEND 
### APPEND ELEMENT TO LIST
Append named or unnamed one element to a list. 
```R
( lst <- list( x = 9 ) )
( lst <- list.append( lst, x = now( ), name = "TIME" ) )
( lst <- list.append( lst, "y", "Ypsilon" ) )
( lst <- list.append( lst, "unnamed" ) )
```

## NOW AND TODAY
### NOW
What's the time now?
```R
now( )
```

### TODAY
What's the date today?
```R
today( )
```

## SUB PLOTS
### GGSUBPLOT
ggsubplot gives the opportunity to create multiplot of ggplots.
```R
hlpr4life::load.pkgs(c(
"hlpr4life",
"ggplot2",
"dplyr" ) )
(d<-data.frame(x=rnorm(1000,10),y=sample(letters[1:3],1000,T),s=sample(c("female","male"),1000,T)))
m<-summarise(group_by(d,y,s),m=mean(x))
ggsubplot(
	ggplot( d ) +
		theme_bw( ) + scale_color_discrete( guide = F ) +
		geom_point( aes( c( 1 : 1000 ), x, col = paste0( y, s ) ) ),
	ggplot( d ) +
		theme_bw( ) + scale_fill_discrete( guide = F ) + scale_color_discrete( guide = F ) +
		geom_histogram( aes( x, fill = paste0( y, s ) ), alpha = .5 ) +
		geom_vline( aes( xintercept = m, col = paste0( y, s ) ), m ) +
		facet_grid( s ~ y ),
	ggplot( d ) +
		theme_bw( ) + scale_fill_discrete( guide = F ) + scale_color_discrete( guide = F ) +
		geom_histogram( aes( x, fill = paste0( y, s ) ), alpha = .5 ) +
		geom_vline( aes( xintercept = m, col = paste0( y, s ) ), m ),
	layout = t(
		matrix(
			c(
				1, 2,
				3, 3 ),
			ncol = 2 ) ) )
```

## SICS
### SOME SICS
Creates a list of sics.
```R
some.sics( n.first = 0, n.last = 110 )
some.sics( n.size = 9 )
some.sics( 10, prefix = "LIFE" )
```

## KEY 
### KEY
key creates a key out of several columns.
```R
key(data.frame(x=letters[ runif(10,1,10)],y=LETTERS[runif(10,1,10)],z=rnorm(10)),c("x","y"),"~")
```

## INFOS FOR MERGING AND LITE DESCRIPTION
### Get merging infos
Could be usefull before a merging process.
```R
(a<-data.frame(SGROUP="A2_02",DATE="200-10-05",Sic="LI12345678"))
(b<-data.frame(GRUPPE="A3_02",DATUM="200-10-05",PSEUDONYM="LI12345678"))
(c<-data.frame(GRP="A2_02",DATE="2001-10-05",EDAT="200-10-04",PSEUDO="LI12345679",edat.new="2017-10-03"))
(infos<-get.merging.infos(c("a","b","c")))
infos$SIC
infos$SCI_GROUP
infos$DATE
```

### Print merging infos
Could be usefull before a merging process.
```R
(a<-data.frame(SGROUP="A2_02",DATE="2002-10-05",Sic="LI12345678"))
(b<-data.frame(GRUPPE="A3_02",DATUM="2002-10-05",PSEUDONYM="LI12345678"))
(c<-data.frame(GRP=c("A2_02","A2_03"),DATE=c("2002-10-05","2001-10-05"),EDAT=c("2001-10-04","2001-10-02"),PSEUDO=c("LI12345679","LI1234567X"),edat.new=c("2017-10-03","2017-10-11")))
print.merging.infos(c("a","b","c"))
```

### SUM AVAILABLES
Count availables in every column of a dataframe!
```R
(d<-data.frame(x=c(NA,"Hello",NA,"World",NA),y=c(1:5),z=rep(NA,5)))
sum.av(d)
```

### SUM MISSINGS
Count missings in every column of a dataframe!
```R
(d<-data.frame(x=c(NA,"Hello",NA,"World",NA),y=c(1:5),z=rep(NA,5)))
sum.na(d)
```

### TABLE DATAFRAME
table.df returns data about missings, availables of every column in a given dataframe. If summary is TRUE, min, max, median and mean are shown additionally.
```R
table.df(data.frame(x=c(1:3),y=c(NA,1,NA),z=c(NA,NA,NA),n=c("blonde","brown","black")))
table.df(data.frame(x=c(1:3),y=c(NA,1,NA),z=c(NA,NA,NA),n=c("blonde","brown","black")),F,F,F)
table.df(data.frame(x=c(1:3),y=c(NA,1,NA),z=c(NA,NA,NA),n=c("blonde","brown","black")),F,T,F)
table.df(data.frame(x=c(1:3),y=c(NA,1,NA),z=c(NA,NA,NA),n=c("blonde","brown","black")),T,F,F)
table.df(data.frame(x=c(1:3),y=c(NA,1,NA),z=c(NA,NA,NA),n=c("blonde","brown","black")),T,T,F)
table.df(data.frame(x=c(1:3),y=c(NA,1,NA),z=c(NA,NA,NA),n=c("blonde","brown","black")),F,F,T)
table.df(data.frame(x=c(1:3),y=c(NA,1,NA),z=c(NA,NA,NA),n=c("blonde","brown","black")),F,T,T)
table.df(data.frame(x=c(1:3),y=c(NA,1,NA),z=c(NA,NA,NA),n=c("blonde","brown","black")),T,F,T)
table.df(data.frame(x=c(1:3),y=c(NA,1,NA),z=c(NA,NA,NA),n=c("blonde","brown","black")),T,T,T)
```

## COLUMNS
### GET COLUMNS
get.columns searches for column names that matches a given the pattern.
```R
(d<-data.frame(SIC="LI01234",Y=1000.,x=10,SCI_GROUP="A2_12",DATE="2017-10-05",EDAT="2017-10-04"))
get.columns(d)
get.columns(d,"S")
```

### GET DATE COLUMNS
get.date.columns is actually a wrapper for get.columns with a given date pattern "edat|date|datum".
```R
(d<-data.frame(DATE="2017-10-05",EDAT="2017-10-04",dat="2017-10-03",DATA="2017-10-02"))
get.date.columns(d)
(d<-data.frame(DATE="2017-10-05",EDAT="2017-10-04",MUTAD="2017-10-03"))
get.date.columns(d,"dat|muta")
```

### GET SIC COLUMNS
get.sic.columns is actually a wrapper for get.columns with a given date pattern "sic|pseudo".
```R
(d<-data.frame(SIC="LI12345678",sic="LI12345679",PSEUDO="LI1234567X",PSEUDONYM="LI12345670"))
get.sic.columns(d)
```

### GET SCI GROUP COLUMNS
get.scigroup.columns is actually a wrapper for get.columns with a given date pattern "sci_group|sci-group|scigroup|sgroup|group|grp|gruppe".
```R
(d<-data.frame(SGROUP="A2_02",SCI_GROUP="B1_10",Gruppe="A1-SK_10",GRP="A3_09"))
get.scigroup.columns(d)
```
