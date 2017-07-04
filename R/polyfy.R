#' polyfy
#'
#' @param x vector of numbers or factors 
#' @param y values of numbers
#' @param g grouping values
#'
#' @return data frame with rows X, Y, G
#' @export
#'
#' @examples
#' polyfy( c( 1 : 100 ), rnorm( 100 ), sample( c( "A", "B" ), 100, TRUE ) )
polyfy <-
    function( x, y, g ) {
        idx <-
            c( 1 : length( x ) )
        z <-
            cumsum( y ) / sum( y )
        d <-
            data.frame(
                X = c( ),
                Y = c( ),
                G = as.factor( c( ) ) )
        grps <-
            unique( g )
        len <-
            length( grps )
        for( i in c( 1 : len ) ) {
            gr <-
                grps[ i ]
            if( i < len && 0 < sum( g == gr ) ) {
                x.g <-
                    c(
                        x[ g == gr ],
                        min( x[ g == grps[ i + 1 ] ] ),
                        min( x[ g == grps[ i + 1 ] ] ),
                        min( x[ g == grps[ i ] ] ) )
                y.g <-
                    c(
                        y[ g == gr ],
                        y[ min( idx[ g == grps[ i + 1 ] ] ) ],
                        0,
                        0 )
            } else {
                x.g <-
                    c(
                        x[ g == gr ],
                        max( x[ g == gr ] ),
                        min( x[ g == gr ] ) )
                y.g <-
                    c(
                        y[ g == gr ],
                        0,
                        0 )
            }
            print( paste( "i", i ) )
            print( gr )
            print( length( x.g ) )
            d <-
                rbind(
                    d,
                    data.frame(
                        X = x.g,
                        Y = y.g,
                        G = rep( gr, length( x.g ) ) ) )
        }
        d
    }
