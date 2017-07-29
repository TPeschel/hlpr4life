#' PLOT COLORS
#'
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
			ggthemes::theme_solid( ) }




