HELPER 4 LIFE

This is a small collection of some usefull R-stuff


# ADJUSTMENTS
## ADJUST LINEARLY 
Adjust a dependent variable y linearly to several dependent variables x1, x2 ...
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

## ADJUST LINEARLY AND STANDARDIZE 
Adjust and standardize afterwards a dependent variable y linearly to several dependent variables x1, x2 ...
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

# PACKAGE LOADER
## LOAD PACKAGES 
Load packages as library() or require() do!
If some are not installed then install them.
```R
load.pkgs( c( "dplyr", "ggplot2", "ggthemes", "reshape2" ) )
```

# CALENDAR
Build a dataframe with calendar data!
```R
clndr <- 
	calendar( 
		start = "2017-01-01", 
		end = "2017-12-31", 
		abbreviate = F, 
		tz = "Europe/Berlin" )

(
	weekends.in.2017 <-
		clndr[ clndr$day_of_week %in% c( "Samstag", "Sonntag" ), ] )

(
	working.days.in.2017 <-
		clndr[ clndr$day.of.week %in% c( 2 : 6 ), ] )

(
	my.birthday <-
		clndr[ clndr$date == "2017-11-28", -2 ] )

(
	summer.2017 <-
		calendar(
			start = "2017-06-21",
			end   = "2017-09-21" ) )
```

# COLORS
## PLOT COLORS
Plot all in R defined colors!
```R
plot.colors( )
```

## COLOR GRADIENT
Build a dataframe with a color gradient between two distinct colors
partitioned by steps!
```R
color.gradient( "red", "green", 16 )
```

## PLOT COLOR GRADIENT
Plot a color gradient between two distinct colors
partitioned by steps!
```R
plot.color.gradient( "red", "green", 256, "horiz", F )
plot.color.gradient( "blue", "yellow", 16, "vert", T )
```

# THE PIANO
## FREQUENCY OF A PIANO KEY'S SOUND
What's the frequency heard by playing key 25 of a piano?
```R
freq.of.key( 25 )
```
## FREQUENCY OF A NOTE
What's the frequency of a certain note?
```R
freq.of.note( "A2" )
```
## KEY OF FREQUENCY
Which key has to be pressed for a certain frequency?
```R
key.of.freq( 110 )
```

## KEY OF NOTE
Which key plays a certain note?
```R
key.of.note( c( "C0", "D0", "E0", "F0", "G0", "A1", "B1", "C1" ) )
```

## NOTE OF FREQUENCY
Which frequency is represented by a certain note?
```R
note.of.freq( 110 )
```

## NOTE OF KEY
Which note has a certain piano key?
```R
note.of.key(key = c(4, 6, 8, 9, 11, 13, 15, 16))
```

## PIANO
Description of a piano
```R
piano(left.key = 4, right.key = 88)
```
```R
load.pkgs( c( "ggplot2", "ggthemes" ) )
piano( 4, 4 + 2 * 12 )
ggplot( piano( 4, 4 + 2 * 12 ) ) +
    geom_histogram( aes( note, -c( 1, .63 )[ match( color, c( "ivory", "ebony" ) ) ], fill = color ), stat = "identity" ) +
    geom_histogram( aes( note, .01 * frequency ), col = "black", stat = "identity" ) +
    scale_fill_manual( values = c( "#000000", "#f0f3f4" ), guide = F ) +
    geom_text( aes( note, label = note, col = color ), y = -.2, angle = 90 ) +
    scale_color_manual( values = c( "#f0f3f4", "#000000" ), guide = F ) +
    theme_solid( fill = "#405060" ) +
    theme(
        axis.text.x = element_blank( ),
        axis.title.x = element_blank( ),
        axis.text.y = element_blank( ),
        axis.title.y = element_blank( ) ) +
    ylim( -1, 1.5 )
```

# RENAMER
## RENAME COLUMNS
Rename some columns of a dataframe.
```R
(d<-rename.columns( data.frame(x=c(1:10),y=rnorm(10),z=c(10:1)),c("y","x"),c("x","y")))
```

Rename some elements of a list.
## RENAME LIST
```R
(l<-rename.list(list(x="x",y="y",z="z"),c("y","z","x"),c("Ypsilon","CED","U")))
```

# LIST APPEND 
## APPEND ELEMENT TO LIST
Append named or unnamed one element to a list. 
```R
( lst <- list( x = 9 ) )
( lst <- list.append( lst, x = now( ), name = "TIME" ) )
( lst <- list.append( lst, "y", "Ypsilon" ) )
( lst <- list.append( lst, "unnamed" ) )
```

# TIME
## TODAY
What date is today?
```R
today( )
```

## NOW
What's the time now?
```R
now( )
```

# SUB PLOTS
## GGSUBPLOT
Ggsubplot gives the opportunity to create multiplot of ggplots.
```R
hlper4life::load.pkgs( c( "hlper4life", "ggplot2" ) )
d <-
	data.frame( 
		x = rnorm( 1000, +10 ), 
		y = sample( letters[ 1 : 3 ], 1000, T ),
		s = sample( c( "female", "male" ), 1000, T ) )
m <- dplyr::summarise(dplyr::group_by(d,y,s),m=mean(x))
ggsubplot(
	ggplot( d ) + theme_bw( ) + scale_color_discrete( guide = F ) +
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
# SOME SICS
Creates a list of sics.
```R
some.sics( n.first = 0, n.last = 110 )
some.sics( n.size = 9 )
some.sics( 10, prefix = "LIFE" )
```

## KEY 
# KEY
Key creates a key out of several columns.
```R
key(data.frame(x=letters[ runif(10,1,10)],y=LETTERS[runif(10,1,10)],z=rnorm(10)),c("x","y"),"~")
```

# INFOS
## MERGING
### Get merging infos
Could be usefull before a merging process.
```R
(a<-data.frame(SGROUP="A2_02",DATE="200-10-05",Sic="LI12345678"))
(b<-data.frame(GRUPPE="A3_02",DATUM="200-10-05",PSEUDONYM="LI12345678"))
(c<-data.frame(GRP="A2_02",DATE="200-10-05",EDAT="200-10-04",PSEUDO="LI12345679",edat.new="2017.10-03"))
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
(c<-data.frame(GRP=c("A2_02","A2_03"),DATE=c("2002-10-05","2001-10-05"),EDAT=c("2001-10-04","200-10-02"),PSEUDO=c("LI12345679","LI1234567X"),edat.new=c("2017.10-03","2017.10-01")))
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
Table availables, missings and if required min, median, mean and max of every column in a dataframe!
```R
table.df(data.frame(x=c(1:3),y=c(NA,1,NA),z=c(NA,NA,NA)))
```



