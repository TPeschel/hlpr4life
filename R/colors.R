#' PLOT COLORS
#'
#' @description a plot of all colors returned by colors( )
#' @return a plot of all colors returned by colors( )
#' @export
#'
#' @examples
#' plot.colors( )
plot.colors <-
	function( ) {
		cols <-
			sort( colors( ) )
		d <-
			data.frame(
				c = colors( ),
				x = rep( seq( 0, 1, length.out = 9 ), 73 ),
				y = as.vector( sapply( c( 0 : 72 ), function( c ) rep( c / 73, 9 ) ) ) )
		ggplot2::ggplot( d ) +
			ggplot2::geom_rect( ggplot2::aes( xmin = x - 1 / 19, xmax = x + 1 / 19, ymin = y - 1 / 147, ymax = y + 1 / 147, fill = c, col = c ) ) +
			# ggplot2::geom_text( ggplot2::aes( x, y, label = c ), nudge_x = -.001, nudge_y = +.001, col = "white", size = 3 ) +
			 ggplot2::geom_text( ggplot2::aes( x, y, label = c ), nudge_x = +.001, nudge_y = -.001, col = "black", size = 3 ) +
			ggplot2::geom_text( ggplot2::aes( x, y, label = c ), col = "gray", size = 3 ) +
			ggplot2::scale_fill_manual( values = cols, guide = F ) +
			ggplot2::scale_color_manual( values = cols, guide = F ) +
			ggthemes::theme_solid( fill = "black" ) }

#' COLOR GRADIENT
#'
#' @description calculates a data frame containing a sequence of colors from one color to a second one and a parallel sequence from 0 to 1
#' @param col.from first color in sequence
#' @param col.to last color in sequence
#' @param length.out length of sequence
#'
#' @return dataframe with color vector, a color sequence from col.from to col.to and sequence from 0 to 1
#' @export
#'
#' @examples
#' color.gradient( "red", "green", 16 )
color.gradient <-
	function( col.from = "red", col.to = "green", length.out = 2 ) {

		if( length.out < 1 ) {

			print( "length.out has to be positive." ) }

		if( length.out < 2 ) {

			return(
				data.frame(
					c = ( col2rgb( col.from ) + col2rgb( col.to ) ) %/% 2,
					x = c( .5 ) ) ) }

		c.f <-
			col2rgb( col.from ) / 255.

		c.t <-
			col2rgb( col.to ) / 255.

		dRGB <-
			c.t - c.f

		cols <-
			rgb(
				t(
					sapply(
						seq( 0, 1, length.out = length.out ),
						function( c ) { c.f + c * dRGB } ) ) )

		cols <-
			factor( x = cols, levels = unique( cols ), labels = unique( cols ) )

		data.frame(
			c = cols,
			x = seq( 0, 1, length.out = length.out ) ) }

#' PLOT COLOR GRADIENT
#'
#' @description plots a gradient picture
#' @param col.from first color
#' @param col.to last color
#' @param length.out number of colors
#' @param dir direction can be "horizontal" or "vertical"
#' @param verbose if true colors are printed inside color rectangles
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' ggsubplot(plot.color.gradient(),plot.color.gradient(dir="h"),cols=2)
plot.color.gradient <-
	function( col.from = "red", col.to ="yellow", length.out = 8, dir = "vertical", verbose = T ) {

		c.g <-
			color.gradient( col.from, col.to, length.out )

		hori <-
			substr( "horizontal", 1, min( nchar( dir ), nchar( "horizontal" ) ) ) == dir

		p <-
			ggplot2::ggplot( c.g ) +
				ggplot2::scale_fill_manual( values = levels( c.g$c ), guide = F ) +
				ggplot2::scale_color_manual( values = levels( c.g$c ), guide = F ) +
				ggthemes::theme_solid( fill =  "black" )

		if( hori ) {

			p <-
				p +
				ggplot2::geom_rect(
					ggplot2::aes(
						xmin = x - .5 / ( length.out - 1 ),
						xmax = x + .5 / ( length.out - 1 ),
						fill = c ),
					ymin = 0,
					ymax = 1 ) }
		else {
			p <-
				p +
				ggplot2::geom_rect(
					ggplot2::aes(
						ymin = x - .5 / ( length.out - 1 ),
						ymax = x + .5 / ( length.out - 1 ),
						fill = c ),
					xmin = 0,
					xmax = 1 ) }

		if( verbose ) {

			if( hori ) {
				p <-
					p +
					ggplot2::geom_text(
						ggplot2::aes(
							x = x + .002,
							label = c ),
						y = .502,
						col = "black",
						angle = 90 ) +
					ggplot2::geom_text(
						ggplot2::aes(
							x = x,
							label = c ),
						y = .5,
						col = "white",
						angle = 90 ) }
			else {
				p <-
					p +
					ggplot2::geom_text(
						ggplot2::aes(
							y = x - .002,
							label = c ),
						x = .502,
						col = "black" ) +
					ggplot2::geom_text(
						ggplot2::aes(
							y = x,
							label = c ),
						x = .5,
						col = "white" )
			} }
		p }
