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
#'
#' hlpr4life::load.pkgs(c("ggplot2","reshape2"))
#' mtcrs<-mtcars
#' mtcrs$am<-factor(mtcrs$am,labels=c("auto","manu"))
#' mtcrs<-adjust.linearly(disp~mpg*am,mtcrs)
#' mtcrs$id<-rownames(mtcrs)
#' mtcrs.mlt<-melt(mtcrs[,c("mpg","cyl","disp","disp.adj.for.mpg.am","am","id")],c("mpg","cyl","am","id"))
#' mtcrs.mlt<-hlpr4life::rename.columns(mtcrs.mlt,c("value","am"),c("displacement","gearbox"))
#' ggplot(mtcrs.mlt,aes(mpg,displacement,col=gearbox,label=paste(id,c("orig","adj")[match(variable,c("disp","disp.adj.for.mpg.am"))]),group=id),mtcrs)+
#' theme_bw()+geom_line()+geom_text(aes(col=paste(gearbox,variable)))+theme(legend.position="none")
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
