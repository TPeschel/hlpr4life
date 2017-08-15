HELPER 4 LIFE

This is a little collection of some usefull R-stuff


# PACKAGE LOADER
## LOAD PACKAGES 
load a package as library or require do.
if there are not installed, then install them,
or some of them.
```R
load.pkgs( c( "dplyr", "ggplot2", "ggthemes", "reshape2" ) )
```
# CALENDAR
build a dataframe with calendar data
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
plot all in R defined colors
```R
plot.colors( )
```
## COLOR GRADIENT
build a dataframe with a color gradient between two distinct colors with a certain step wide
```R
color.gradient( "red", "green", 16 )
```
## PLOT COLOR GRADIENT
plot a color gradient between two distinct colors with a certain step wide
```R
plot.color.gradient( "red", "green", 256, "horiz", F )
plot.color.gradient( "blue", "yellow", 16, "vert", T )
```

# THE PIANO
## FREQUENCY OF A PIANO KEY'S SOUND
what's the frequency heard by playing key 25 of a piano?
```R
freq.of.key( 25 )
```
## FREQUENCY OF A NOTE
what's the frequency of a certain note
```R
freq.of.note( "A2" )
```
## KEY OF FREQUENCY
which key has to be pressed for a certain frequency?
```R
key.of.freq( 110 )
```
## KEY OF NOTE
which key plays a certain note?
```R
key.of.note( c( "C0", "D0", "E0", "F0", "G0", "A1", "B1", "C1" ) )
```
## NOTE OF FREQUENCY
which frequency is represented by a certain note?
```R
note.of.freq( 110 )
```
## NOTE OF KEY
which note has a certain piano key?
```R
note.of.key(key = c(4, 6, 8, 9, 11, 13, 15, 16))
```
## PIANO
```R
piano(left.key = 4, right.key = 88)
```
```R
load.pkgs( c( "ggplot2", "ggthemes" ) )
piano( 4, 4 + 2 * 12 )
ggplot( piano( 4, 4 + 2 * 12 ) ) +
    geom_histogram( aes( note, -c( 1, .63 )[ match( color, c( "ivory", "ebony" ) ) ], fill = color ), stat = "identity" ) +
    scale_fill_manual( values = c( "#000000", "#f0f3f4" ), guide = F ) +
    geom_text( aes( note, label = note, col = color ), y = -.2, angle = 90 ) +
    scale_color_manual( values = c( "#f0f3f4", "#000000" ), guide = F ) +
    theme_solid( fill = "#405060" ) +
    theme(
        axis.text.x = element_blank( ),
        axis.title.x = element_blank( ),
        axis.text.y = element_blank( ),
        axis.title.y = element_blank( ) ) +
    ylim( -1, 0 )
```
# RENAMER
## RENAME COLUMN
```R
(d<-rename.column(data.frame(x=c(1:10),y=rnorm(10)),"x","X"))
(d<-rename.column(d,"y","Y"))
```
## RENAME COLUMNS
```R
(d<-rename.columns( data.frame(x=c(1:10),y=rnorm(10),z=c(10:1)),c("y","x"),c("x","y")))
```
# SUB PLOTS
## GGSUBPLOT
```R
load.pkgs( c( "ggplot2" ) )
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
