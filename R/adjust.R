#' ADJUST
#'
#' @param formula A formula that gives the relationship of the adjustment.
#' @param data A dataframe which contains the dependent and the independent variables.
#'
#' @return A dataframe containing a new column with the adjusted values.
#' @export
#'
#' @examples
#'
#' hlpr4life::load.pkgs(c("ggplot2","reshape2"))
#' mtcrs<-mtcars
#' mtcrs$am<-factor(mtcrs$am,labels=c("auto","manu"))
#' mtcrs<-adjust(disp~mpg*am,mtcrs)
#' mtcrs$id<-rownames(mtcrs)
#' mtcrs.mlt<-melt(mtcrs[,c("mpg","cyl","disp","disp.adj.for.mpg.am","am","id")],c("mpg","cyl","am","id"))
#' mtcrs.mlt<-hlpr4life::rename.columns(mtcrs.mlt,c("value","am"),c("displacement","gearbox"))
#' ggplot(mtcrs.mlt,aes(mpg,displacement,col=gearbox,label=paste(id,c("orig","adj")[match(variable,c("disp","disp.adj.for.mpg.am"))]),group=id),mtcrs)+
#' theme_bw()+geom_line()+geom_text()
adjust <-
    function( formula, data ) {

        a.v <-
            all.vars( formula )

        nm <-
            paste0( a.v[ 1 ], ".adj.for" )

        for( a in a.v[ -1 ] )

            nm <-
                paste0( nm, ".", a )

        data[ , nm ] <-
            ( lm( formula, data ) )$residuals + mean( data[ , a.v[ 1 ] ], na.rm = T )

        data }
########################################################################
#' ADJUST and STANDARIZE
#'
#' @param formula A formula that gives the relationship of the adjustment.
#' @param data A dataframe which contains the dependent and the independent variables.
#'
#' @return A dataframe containing a new column with the adjusted and standardized values.
#' @export
#'
#' @examples
#'
#' hlpr4life::load.pkgs(c("ggplot2","reshape2"))
#' mtcrs<-mtcars
#' mtcrs$am<-factor(mtcrs$am,labels=c("auto","manu"))
#' mtcrs<-adjust.std(disp~mpg*am,mtcrs)
#' mtcrs$id<-rownames(mtcrs)
#' mtcrs.mlt<-melt(mtcrs[,c("mpg","cyl","disp","disp.adj.for.mpg.am","am","id")],c("mpg","cyl","am","id"))
#' mtcrs.mlt<-hlpr4life::rename.columns(mtcrs.mlt,c("value","am"),c("displacement","gearbox"))
#' ggplot(mtcrs.mlt,aes(mpg,displacement,col=gearbox,label=paste(id,c("orig","adj")[match(variable,c("disp","disp.adj.for.mpg.am"))]),group=id),mtcrs)+
#' theme_bw()+geom_line()+geom_text()
adjust.std <-
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
