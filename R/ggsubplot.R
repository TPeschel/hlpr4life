#' GGSUBPLOT
#'
#' @name ggsubplot
#' @description gives the opportunity for subplots. it has nothing to do with ggplot exept of the fact, that it is build for ggplots. It is taken from R-Cookbook.
#'
#' @param ... end of list of ggplots
#' @param plotlist end of list of ggplots
#' @param cols count of columns
#' @param layout matrix for the layout with indices of the plots
#'
#' @return subplots
#' @export
#'
#' @examples
#' hlpr4life::load.pkgs( c( "hlpr4life", "ggplot2" ) )
#' (d<-data.frame(x=rnorm(1000,10),y=sample(letters[1:3],1000,T),s=sample(c("female","male"),1000,T))
#' m<-dplyr::summarise(dplyr::group_by(d,y,s),m=mean(x))
#' ggsubplot(
#'   ggplot( d ) + theme_bw( ) + scale_color_discrete( guide = F ) +
#'     geom_point( aes( c( 1 : 1000 ), x, col = paste0( y, s ) ) ),
#'   ggplot( d ) +
#'     theme_bw( ) + scale_fill_discrete( guide = F ) + scale_color_discrete( guide = F ) +
#'     geom_histogram( aes( x, fill = paste0( y, s ) ), alpha = .5 ) +
#'     geom_vline( aes( xintercept = m, col = paste0( y, s ) ), m ) +
#'     facet_grid( s ~ y ),
#'   ggplot( d ) +
#'     theme_bw( ) + scale_fill_discrete( guide = F ) + scale_color_discrete( guide = F ) +
#'     geom_histogram( aes( x, fill = paste0( y, s ) ), alpha = .5 ) +
#'     geom_vline( aes( xintercept = m, col = paste0( y, s ) ), m ),
#'   layout = t(
#'     matrix(
#'       c(
#'         1, 2,
#'         3, 3 ),
#'       ncol = 2 ) ) )
ggsubplot <-
    function(
    	...,
    	plotlist = NULL,
    	cols = 1,
    	layout = NULL ) {

        # Make a list from the ... arguments and plotlist
        plots <-
            c( list(...), plotlist )

        numPlots <- length( plots )

        # If layout is NULL, then use 'cols' to determine layout
        if( is.null( layout ) ) {
            # Make the panel
            # ncol: Number of columns of plots
            # nrow: Number of rows needed, calculated from # of cols
            layout <-
                matrix(
                    seq(
                        1,
                        cols * ceiling( numPlots / cols ) ),
                    ncol = cols,
                    nrow = ceiling( numPlots / cols ) )
          }

         if( numPlots == 1 ) {
             print( plots[[ 1 ]] )

          } else {
              # Set up the page
              grid::grid.newpage( )
              grid::pushViewport(
                  grid::viewport(
                      layout = grid::grid.layout(
                          nrow( layout ),
                          ncol( layout ) ) ) )
              # Make each plot, in the correct location
              for( i in 1 : numPlots ) {
                  # Get the i,j matrix positions of the regions that contain this subplot
                  matchidx <-
                      as.data.frame(
                          which(
                              layout == i,
                              arr.ind = TRUE ) )

                  print(
                      plots[[ i ]],
                      vp = grid::viewport(
                          layout.pos.row = matchidx$row,
                          layout.pos.col = matchidx$col ) ) } } }
