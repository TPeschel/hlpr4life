#' load.pkgs
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
	function( pkgs ) {
		for( p in pkgs ) {
			if( !require( p, character.only = TRUE ) ) {
				install.packages( p )
				library( p, character.only = TRUE ) } } }
