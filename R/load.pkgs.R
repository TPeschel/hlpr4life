#' LOAD PACKAGES
#' @name load.pkgs
#'
#' @description load packages tries to load a package into the environment. If it fails, it tries to install the package and loads it afterwards.
#' @param pkgs
#'
#' @export
#'
#' @examples
#' load.pkgs( c( "dplyr", "ggplot2", "ggthemes", "reshape2" ) )
load.pkgs <-
	function( pkgs =c( "dplyr", "ggplot2", "ggthemes", "reshape2" )) {

		exist <-
			pkgs %in% rownames( installed.packages( ) )

		if( any( !exist ) )
			install.packages( pkgs[ !exist ] )

		sapply( pkgs, library, character.only = TRUE ) }
