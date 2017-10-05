#' KEY OF FREQUENCY
#'
#' @description key.of.freq returns the piano key that plays a tone of the frequency frq when its first (most left) key plays tone "A0"
#' @param frq frequency of the key
#'
#' @return key of frq
#' @export key.of.freq( )
#'
#' @examples
#' key.of.freq( 110 )
key.of.freq <-
    function( frq = 110 ) {
        log2( frq / 440 ) * 12 + 49 }

#' FREQUENCY OF KEY
#'
#' @description freq.of.key returns the frequency that a piano key plays which first (most left) key plays tone "A0"
#' @param key
#' key for what the frequency is asked for,
#' key number of a piano,
#' most left is 1
#'
#' @return frequency of the key
#' @export
#'
#' @examples
#' freq.of.key( 25 )
freq.of.key <-
    function( key = 25 ) {
        440 * 2 ** ( ( key - 49 ) / 12 ) }

#' NOTE OF KEY
#'
#' @description note.of.key gives the note for a certain piano key which first (most left) key plays tone "A0"
#' @param key
#' key for what the note is asked for,
#'
#' @return note of key
#' @export
#'
#' @examples
#' note.of.key( 25 )
note.of.key <-
    function( key = c( 4, 6, 8, 9, 11, 13, 15, 16 ) ) {
        paste0(
            c(
                "A", "A#",
                "B",
                "C", "C#",
                "D", "D#",
                "E",
                "F", "F#",
                "G", "G#" )[ 1 + ( ( key - 1 ) %% 12 ) ],
            ( key - 1 ) %/% 12 ) }

#' KEY OF NOTE
#'
#' @description key.of.note gives the note for a certain piano key which first (most left) key plays tone "A0"
#' @param note
#' note for what the key is asked for
#' @return key of note
#' @export
#'
#' @examples
#' key.of.note( c( "C0", "D0", "E0", "F0", "G0", "A1", "B1", "C1" ) )
key.of.note <-
    function( note ) {
        base <-
            c(
                "A", "A#",
                "B",
                "C", "C#",
                "D", "D#",
                "E",
                "F", "F#",
                "G", "G#" )
        a <-
            match(
                stringr::str_extract( note, "[A-Z]+#*"),
                base )
        b <-
            as.numeric( stringr::str_extract( note, "[0-9]$") )
        a + b * 12 }

#' NOTE OF FREQUENCY
#'
#' @description note.of.freq gives the note for a certain frequency
#' @param frq
#' frequency for what the note is asked for
#'
#' @return note of frequency
#' @export
#'
#' @examples
#' note.of.freq( 110 )
note.of.freq <-
    function( frq = 110 ) {
        note.of.key( key.of.freq( frq ) ) }

#' FREQUENCY OF NOTE
#'
#' @description freq.of.note gives the frequency for a certain note
#' @param note
#' note for what the frequency is asked for,
#'
#' @return frequency of note
#' @export
#'
#' @examples
#' freq.of.note( "A2" )
freq.of.note <-
    function( note ) {
        freq.of.key( key.of.note( note ) ) }

#' PIANO
#'
#' @description piano gives a data.frame of a piano which first (most left) key plays tone "A0".
#' One can expand or shrink the keyboard by giving the piano's left.key, right.key.
#' The returned dataframe then contains all keys between left.key and right.key, their colors, notes and played frequencies.
#'
#' @param left.key left key of piano
#' @param right.key right key of piano
#'
#' @return piano with keys, their colors, notes and frequencies
#' @export piano()
#'
#' @examples
#' load.pkgs( c( "ggplot2", "ggthemes" ) )
#' piano( 4, 4 + 2 * 12 )
#' ggplot( piano( 4, 4 + 2 * 12 ) ) +
#'     geom_histogram( aes( note, -c( 1, .63 )[ match( color, c( "ivory", "ebony" ) ) ], fill = color ), stat = "identity" ) +
#'     scale_fill_manual( values = c( "#000000", "#f0f3f4" ), guide = F ) +
#'     geom_text( aes( note, label = note, col = color ), y = -.2, angle = 90 ) +
#'     scale_color_manual( values = c( "#f0f3f4", "#000000" ), guide = F ) +
#'     theme_solid( fill = "#405060" ) +
#'     theme(
#'         axis.text.x = element_blank( ),
#'         axis.title.x = element_blank( ),
#'         axis.text.y = element_blank( ),
#'         axis.title.y = element_blank( ) ) +
#'     ylim( -1, 0 )
piano <-
    function( left.key = 4, right.key = 88 ) {
        k <-
            c( left.key : right.key )
        data.frame(
            key   = k,
            color = c( "ebony", "ivory" )[ match( grepl( "#", note.of.key( k ) ), c( T, F ) ) ],
            note  = factor(
                x      = k,
                levels = k,
                labels = note.of.key( k ) ),
            frequency  = freq.of.key( k ) ) }

##
# example:
# all keys with their colors, notes, frequencies of a common piano
##
