HELPER 4 LIFE

This is a little collection of some usefull R-stuff

# CALENDAR

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
```R
plot.colors( )
```
## COLOR GRADIENT
```R
color.gradient( "red", "green", 16 )
```
## PLOT COLOR GRADIENT
```R
plot.color.gradient( "red", "green", 256, "horiz", F )
plot.color.gradient( "blue", "yellow", 16, "vert", T )
```
