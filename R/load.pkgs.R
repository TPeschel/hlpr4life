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

		missing.pkgs <-
			pkgs[ !pkgs %in% installed.packages( ) ]

		for( p in missing.pkgs ) {

			print( paste0( "install package ", p, "." ) )

			install.packages( p ) }

		sapply( pkgs, library, character.only = TRUE ) }
