#' ADJUST LINEARLY
#'
#' @description Adjust a dependent variable y linearly to several dependent variables x1, x2 ...
#' @param formula A formula that gives the relationship of the adjustment.
#' @param data A dataframe which contains the dependent and the independent variables.
#'
#' @return A dataframe containing a new column with the adjusted values.
#' @export
#'
#' @examples
#' hlpr4life::load.pkgs( c( "hlpr4life", "ggplot2" ) )
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

        data }
########################################################################
#' ADJUST LINEARLY and STANDARIZE
#'
#' @description Adjust a dependent variable y linearly to several dependent variables x1, x2 ...
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

        data }
