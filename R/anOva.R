library( ggplot2 )

COLORS <-
	c(
		"#90f000", "#0050a0", "#f83792", "#9030f0", "#40d040", "#306080", "#a04000",
		"#202020", "#404040", "#606060", "#808080", "#a0a0a0", "#c0c0c0", "#e0e0e0",
		"#f02020", "#f04040", "#f06060", "#f08080", "#f0a0a0", "#f0c0c0", "#f0e0e0",
		"#20f020", "#40f040", "#60f060", "#80f080", "#a0f0a0", "#c0f0c0", "#e0f0e0",
		"#2020f0", "#4040f0", "#6060f0", "#8080f0", "#a0a0f0", "#c0c0f0", "#e0e0f0",
		"#f0f020", "#f0f040", "#f0f060", "#f0f080", "#f0f0a0", "#f0f0c0", "#f0f0e0",
		"#20f0f0", "#40f0f0", "#60f0f0", "#80f0f0", "#a0f0f0", "#c0f0f0", "#e0f0f0",
		"#f020f0", "#f040f0", "#f060f0", "#f080f0", "#f0a0f0", "#f0c0f0", "#f0e0f0",
		"#313792", "#903030", "#903000", "#0050a0", "#40d040", "#306010", "#a04000",
		"#202020", "#404040", "#606060", "#101010", "#a0a0a0", "#c0c0c0", "#e0e0e0",
		"#302020", "#304040", "#306060", "#301010", "#30a0a0", "#30c0c0", "#30e0e0",
		"#203020", "#403040", "#603060", "#103010", "#a030a0", "#c030c0", "#e030e0",
		"#202030", "#404030", "#606030", "#101030", "#a0a030", "#c0c030", "#e0e030",
		"#303020", "#303040", "#303060", "#303010", "#3030a0", "#3030c0", "#3030e0",
		"#203030", "#403030", "#603030", "#103030", "#a03030", "#c03030", "#e03030",
		"#302030", "#304030", "#306030", "#301030", "#30a030", "#30c030", "#30e030" )

my.cols <-
	scale_color_manual(
		values =
			COLORS )

my.fills <-
	scale_fill_manual(
		values =
			COLORS )

# default theme for all plots
my.theme <-
    theme_bw( ) +
	theme(
    	axis.title.x = element_text( angle = 0, hjust = .5 ),
    	axis.title.y = element_text( angle = 0, vjust = .5 )
    	#panel.grid = element_blank( )
    	)

# polyfy
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

# residuals
res.iduals <-
	function( v ) {
		m <- mean( v )
		v - m
	}
# chi square distribution of v
chi.sq <-
	function( v ) res.iduals( v ) ** 2

# lttle helper SUM OF SQUARES
sum.of.squares <-
    function( v ) sum( chi.sq( v ) )

# summarise and aggregate the groups
my.summary <-
	function( d ) {

		d %>%
		    group_by( GROUP ) %>%
		    summarise(
		        COUNT    = n( ),
		        MEAN     = mean( Y ),
		        VAR      = var( Y ),
		        SIGMA    = sd( Y ),
				SQR      = sum.of.squares( Y ),
		        ERR.LOW  = MEAN - SIGMA,
		        ERR.HIGH = MEAN + SIGMA,
		    	P.010    = quantile( probs = .01, x = Y ),
		    	P.025    = quantile( probs = .025, x = Y ),
		    	P.050    = quantile( probs = .05, x = Y ),
		    	P.100    = quantile( probs = .1, x = Y ),
		    	P.250    = quantile( probs = .25, x = Y ),
		    	P.500    = quantile( probs = .50, x = Y ),
		    	P.750    = quantile( probs = .75, x = Y ),
		    	P.900    = quantile( probs = .90, x = Y ),
		    	P.950    = quantile( probs = .95, x = Y ),
		    	P.975    = quantile( probs = .975, x = Y ),
		    	P.990    = quantile( probs = .99, x = Y ) )
	}

plot.an.ova <-
	function( an.O.va ) {

		d <-
			an.O.va$DATA

		e <-
			an.O.va$DESCRIPTION

		d$COUNT <-
			e$COUNT[ d$GROUP ]

		n <-
			nrow( d )

		list(

			scatter =
				function( ) {
					ggplot( ) +
						my.theme + my.fills + my.cols +
						geom_point( data = d, aes( TIME, Y ), alpha = .2 ) +
						geom_hline( data = d, aes( yintercept = mean( Y ) ) ) },

			scatter.groups =
				function( ) {
					ggplot( ) +
						my.theme + my.fills + my.cols +
						geom_point( data = d, aes( TIME, Y, col = GROUP ), alpha = .3 ) +
						geom_hline( data = d, aes( yintercept = mean( Y ) ) ) +
						geom_hline( data = e, aes( yintercept = MEAN, col = GROUP ) ) },

			density.dodge =
				function( ) {
					ggplot( ) +
						my.theme + my.fills + my.cols +
						geom_vline( data = e, aes( xintercept = MEAN, col = GROUP ) ) +
						geom_vline( data = d, aes( xintercept = mean( Y ) ) ) +
						geom_density( data = d, aes( Y, col = GROUP, fill = GROUP ), position = "dodge", alpha = .2 ) },

			density.stack =
				function( ) {
					ggplot( ) +
						my.theme + my.fills +my.cols +
						geom_vline( data = e, aes( xintercept = MEAN, col = GROUP ) ) +
						geom_density( data = d, aes( Y, col = GROUP, fill = GROUP ), position = "stack", alpha = .5 ) },

			density.facet =
				function( ) {
					ggplot( ) +
						my.theme + my.fills + my.cols +
						geom_vline( data = e, aes( xintercept = MEAN ), col = "black", size = 2 ) +
						geom_density( data = d, aes( Y, col = GROUP, fill = GROUP, alpha = COUNT ), position = "dodge" ) +
						geom_vline( data = e, aes( xintercept = P.010 ) ) +
						geom_vline( data = e, aes( xintercept = P.025 ) ) +
						geom_vline( data = e, aes( xintercept = P.100 ) ) +
						geom_vline( data = e, aes( xintercept = P.250 ) ) +
						geom_vline( data = e, aes( xintercept = P.050 ) ) +
						geom_vline( data = e, aes( xintercept = P.750 ) ) +
						geom_vline( data = e, aes( xintercept = P.900 ) ) +
						geom_vline( data = e, aes( xintercept = P.950 ) ) +
						geom_vline( data = e, aes( xintercept = P.975 ) ) +
						geom_vline( data = e, aes( xintercept = P.990 ) ) +
						facet_grid( GROUP ~ . ) },

			histogram =
				function( ) {
					ggplot( d ) +
						my.theme + my.fills + my.cols +
						geom_histogram( aes( cut( Y, c( -Inf, +.5 + c( floor( min( Y ) ) : floor( max( Y ) ) ) ), labels = c( floor( min( Y ) ) : floor( max( Y ) ) ) ), fill = GROUP ), col = "#808080", bins = 10, alpha = 1, position = "stack", stat = "count" ) +
						geom_histogram( aes( cut( Y, c( -Inf, +.5 + c( floor( min( Y ) ) : floor( max( Y ) ) ) ), labels = c( floor( min( Y ) ) : floor( max( Y ) ) ) ), fill = GROUP ), col = "white", bins = 10, alpha = 1, position = "dodge", stat = "count" ) },

			histogram.facet =
				function( ) {
					ggplot( d ) +
						my.theme + my.fills + my.cols +
						geom_histogram( aes( cut( Y, c( -Inf, +.5 + c( floor( min( Y ) ) : floor( max( Y ) ) ) ), labels = c( floor( min( Y ) ) : floor( max( Y ) ) ) ), fill = GROUP ), col = "#808080", bins = 10, alpha = 1, position = "stack", stat = "count" ) +
						geom_histogram( aes( cut( Y, c( -Inf, +.5 + c( floor( min( Y ) ) : floor( max( Y ) ) ) ), labels = c( floor( min( Y ) ) : floor( max( Y ) ) ) ), fill = GROUP ), col = "white", bins = 10, alpha = 1, position = "dodge", stat = "count" ) +
						facet_grid( GROUP ~ . ) },

			boxplot =
				function( ) {
					ggplot( ) +
					    my.theme + my.cols + my.fills +
				    	geom_boxplot( data = d, aes( x = GROUP, y = Y ), notch = F, outlier.alpha = 1, outlier.color = "red", outlier.size = 2, alpha = .2, size = .5 ) +
				    	geom_point( data = d, aes( x = GROUP, y = Y, col = GROUP ), alpha = .2, position = "jitter" ) +
				    	geom_errorbar( data = e, aes( x = GROUP, ymin = P.010, ymax = P.990 ), width = exp( .010 ) - .9, size = .25, alpha = 1. ) +
				    	geom_errorbar( data = e, aes( x = GROUP, ymin = P.025, ymax = P.975 ), width = exp( .025 ) - .9, size = .50, alpha = .8 ) +
				    	geom_errorbar( data = e, aes( x = GROUP, ymin = P.050, ymax = P.950 ), width = exp( .050 ) - .9, size = .75, alpha = .6 ) +
				    	geom_errorbar( data = e, aes( x = GROUP, ymin = P.100, ymax = P.900 ), width = exp( .100 ) - .9, size = 1, alpha = .4 ) +
				    	geom_errorbar( data = e, aes( x = GROUP, ymin = P.250, ymax = P.750 ), width = exp( .250 ) - .9, size = 2, alpha = .2 ) +
				    	geom_errorbar( data = e, aes( x = GROUP, ymin = P.500, ymax = P.500 ), width = exp( .500 ) - .9, size = 4, alpha = .1 ) +
				    	geom_errorbar( data = e, aes( x = GROUP, ymin = MEAN, ymax = MEAN, col = GROUP ), width = 1, size = 1, alpha = 1 ) +
				    	geom_errorbar( data = e, aes( x = GROUP, ymin = ERR.LOW, ymax = ERR.HIGH, col = GROUP ), width = .68, size = .68, alpha = .68 ) +
				    	geom_text( data = e, aes( x = GROUP, y = MEAN + ( P.750 - MEAN ) / 2, label = round( MEAN, 2 ) ), nudge_x = .25, nudge_y = 0 ) +
						geom_point( data = e, aes( x = GROUP, y = MEAN, size = COUNT ), col = "black", alpha = 1, shape = 7 )
					},

			scatter.residual =
				function( ) {
					ggplot( ) +
						my.theme + my.fills + my.cols +
						geom_point( data = d, aes( TIME, chi.sq( Y ) ), alpha = .2 ) +
						geom_hline( data = d, aes( yintercept = mean( Y ) ) ) },

			scatter.residual.groups =
				function( ) {
					ggplot( ) +
						my.theme + my.fills + my.cols +
						geom_point( data = d, aes( TIME, chi.sq( Y ), col = GROUP ), alpha = .2 ) +
						geom_hline( data = d, aes( yintercept = mean( Y ) ) ) +
						facet_grid( GROUP ~ . ) },

			histogram.residual =
				function( ) {
					ggplot( ) +
						my.theme + my.fills + my.cols +
						geom_histogram( data = d, aes( chi.sq( Y ) ) ) },

			histogram.residual.groups =
				function( ) {
					ggplot( ) +
						my.theme + my.fills + my.cols +
						geom_histogram( data = d, aes( chi.sq( Y ), fill = GROUP ) ) +
						facet_grid( GROUP ~ . ) },

			density.residual =
				function( ) {
					ggplot( ) +
						my.theme + my.fills + my.cols +
						geom_density( data = d, aes( chi.sq( Y ), inherit.aes = F, fill = GROUP ), alpha = 1 ) +
						geom_density( data = d, aes( chi.sq( Y ) ), inherit.aes = F, fill = "gray", alpha = .5, col = "gray", size = 2 ) },

			density.residual.groups =
				function( ) {
					ggplot( ) +
						my.theme + my.fills + my.cols +
						geom_density( data = d, aes( chi.sq( Y ), fill = GROUP ) ) +
						facet_grid( GROUP ~ . ) },

			f.plot =
				function( ) {

					f.sim <-
						rf( 100000, an.O.va$ANOVA$DF.SQE, an.O.va$ANOVA$DF.SQR );

					f.sim.low  <-
						f.sim[ f.sim < an.O.va$ANOVA$F.VALUE ]

					f.sim.high <-
						f.sim[ an.O.va$ANOVA$F.VALUE <= f.sim ]

					ggplot( ) +
						my.theme + my.fills + my.cols +
						geom_histogram( aes( f.sim.low ), binwidth = .05, fill = "green" ) +
						geom_histogram( aes( f.sim.high ), binwidth = .05,   fill = "red" ) +
						geom_vline( xintercept = an.O.va$ANOVA$F.VALUE, "yellow" ) +
						geom_text( aes( x = max( an.O.va$ANOVA$F.VALUE, f.sim, na.rm = T ) / 2, y = 300, label = paste0( "F = ", round( an.O.va$ANOVA$F.VALUE, 4 ), "\n  p = ", round( an.O.va$ANOVA$P.VALUE, 4 ) ) ) ) } )
		}

anOva <-
	function( y, g ) {

		data.grp.tm.y <-
			data.frame(
				Y = y,
				GROUP = g,
				TIME = seq_along( y ) )
		dscrptn <-
			my.summary(
				data.frame(
					Y = data.grp.tm.y$Y,
					GROUP = data.grp.tm.y$GROUP ) )
		sqr =
			sum( dscrptn$SQR )

		sqt =
			sum.of.squares( data.grp.tm.y$Y )

		sqe = sqt - sqr

		df.e <-
			length( levels( data.grp.tm.y$GROUP ) ) - 1

		df.r <-
			length( data.grp.tm.y$Y ) - df.e - 1

		mean.sqe = sqe / df.e
		mean.sqr = sqr / df.r

		f.value = mean.sqe / mean.sqr

		list(
			DESCRIPTION =
				dscrptn,
			ANOVA =
				data.frame(
					SQT =
						sqt,
					SQR =
						sqr,
					SQE =
						sqe,
					DF.SQR =
						df.r,
					DF.SQE =
						df.e,
					MEAN.SQR =
						mean.sqr,
					MEAN.SQE =
						mean.sqe,
					F.VALUE =
						f.value,
					P.VALUE =
						1 - pf( f.value, df.e, df.r )
					),
			DATA =
				data.grp.tm.y ) }

table.an.ova <-
	function( d ) {
		table(
			d$DATA$GROUP,
			cut(
				d$DATA$Y,
				breaks =
					c( floor( min( d$DATA$Y ) - 1 ) : floor( max( d$DATA$Y ) ) ),
				labels =
					c( floor( min( d$DATA$Y ) ) : floor( max( d$DATA$Y ) ) ) ) ) }
