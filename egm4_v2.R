#!/usr/bin/env Rscript

# R script to process EGM-4 data
# BBL April 2014

# Support functions and common definitions

args = commandArgs( trailingOnly = TRUE )

SCRIPTNAME		<- "egm4_v2.R"
OUTPUT_DIR		<- "outputs"
LOG_DIR			<- "logs"
SEPARATOR		<- "-------------------"
MEAS_INTERVAL	<- 10
HEIGHT_OPTIONS	<- args[2]
INPUT_DIR		<- args[1]

files <- args[-( 1:2 )]
	

printfiles <- function( files ) {
	cat( "Reading the following files:" )
	for ( file in files )
		cat(file, sep = "\n")
}

# -----------------------------------------------------------------------------
# Time-stamped output function
printlog <- function( msg="", ..., ts=TRUE, cr=TRUE ) {
	if( ts ) cat( date(), " " )
	cat( msg, ... )
	if( cr ) cat( "\n")
} # printlog

# -----------------------------------------------------------------------------
# Print dimensions of data frame
printdims <- function( d, dname=deparse( substitute( d ) ) ) {
	stopifnot( is.data.frame( d ) )
	printlog( dname, "rows =", nrow( d ), "cols =", ncol( d ) )
} # printdims

# -----------------------------------------------------------------------------
# Save a data frame
savedata <- function( df, extension=".csv" ) {
	stopifnot( file.exists( OUTPUT_DIR ) )
	fn <- paste0( OUTPUT_DIR, "/", deparse( substitute( df ) ), extension )
	printlog( "Saving", fn )
	write.csv( df, fn, row.names=F )
} # savedata

get_height <- function( ){
	heights <- paste0( INPUT_DIR, HEIGHT_OPTIONS )
	stopifnot( file.exists( heights ) )
	h <- read.table( heights, comment.char=";",sep="\t" )

	names( h ) <- c("Plot", "Height")
	arrange(h, Plot)

	h
}

# -----------------------------------------------------------------------------
# Load requested libraries
loadlibs <- function( liblist ) {
	printlog( "Loading libraries..." )
	loadedlibs <- vector()
	for( lib in liblist ) {
		printlog( "Loading", lib )
		loadedlibs[ lib ] <- require( lib, character.only=T )
		if( !loadedlibs[ lib ] )
			warning( "this package is not installed!" )
	}
	invisible( loadedlibs )
} # loadlibs

#clean_egm <- function ( fn ) {
#	fqfn <- paste0( INPUT_DIR, fn )
#	stopifnot( file.exists( fqfn ) )
#	d <- read.table( fqfn, comment.char=";", sep="\t" )
#	for (row in d.ro)
#}

# -----------------------------------------------------------------------------
# read a process a single EGM4 output file, returning data frame
read_egmfile <- function( fn ) {
	fqfn <- paste0( INPUT_DIR, fn )
	printlog( "Reading", fqfn )
	stopifnot( file.exists( fqfn ) )
	d <- read.table( fqfn, comment.char=";", sep="\t", na.strings="")
	d <- d[1:19]
	printdims( d )
	names( d ) <- c( "Plot", "RecNo", "Day", "Month", "Hour", "Min", "CO2_Ref", "mb_Ref",
		 "mbR_Temp", "Input_A", "Input_B", "Input_C", "Input_D", "Input_E", "Input_F", 
		 "Input_G", "Input_H", "ATMP", "Probe Type" )
	d$filename <- fn
	printlog( "Adding seconds (interval =", MEAS_INTERVAL, ")" )
	d <- ddply( d, .( Plot ), mutate, Sec=seq( from=0, length.out=length( Plot ), by=MEAS_INTERVAL ) )
	
	# QC
	printlog( "Computing CO2~Time R2 values for quality control..." )
	mods <- dlply( d, .( Plot ), lm, formula = CO2_Ref ~ Sec )
	r2 <- ldply( mods, .fun=function( x ){ round( summary( x )$r.squared, 2 ) } )
	names( r2 ) <- c( "Plot", "R2" )
	r2 <- r2[ order( r2$R2 ), ]
	print( r2 )

	d
} # read_egmfile

# -----------------------------------------------------------------------------
# compute fluxes
# TODO: 
compute_flux <- function( d ) {

	m <- lm( CO2_Ref ~ Sec, data=d )
	Resp_raw <- as.numeric( coef( m )[ 2 ] )	# i.e. the slope
	
	# We want to convert raw respiration (d[CO2]/dt) to a flux using
	# A = dC/dt * V/S * Pa/RT (e.g. Steduto et al. 2002), where
	# 	A is CO2 flux (umol/m2/s)
	#	dC/dt is raw respiration as above (mole fraction/s)
	# 	V is total chamber volume (m3)
	#		...we are correcting for varying headspaces in the cores
	#	S is ground surface area (m2)
	#		...but we're computing per kg of soil, so using dry mass instead
	#	Pa is atmospheric pressure (kPa)
	#	R is universal gas constant (8.3 x 10-3 m-3 kPa mol-1 K-1)
	#	T is air temperature (K)

	ring_r 		<- 5									# diameter, cm
	sleeve_ht	<- as.numeric( d$Height[ 1 ] ) 			# height, cm
	egm4_vol	<- 2427									# internal system volume, cm3
	S 			<- pi * ring_r ^ 2						# note cm2, not m2!
	sleeve_vol 	<- sleeve_ht * S 							# m3
	V			<- ( egm4_vol + sleeve_vol ) / 100^3	# m3
	R 			<- 7.436e-3								# m-3 kPa mol-1 K-1
	Kelvin		<- 273.15								#C to K conversion
	avg_temp 	<- mean( d$Input_C )					# assumes EGM temperature probe connected

	# Convert from umol/g soil/s to mgC/kg soil/day
	R_ewd <- Resp_raw * (V / S) * R * ( Kelvin / (Kelvin + avg_temp ) )

	# Calculate the flux in uMol CO2 m^2 / s 
	flux <- R_ewd / 1e6 * 12 * ( 1000 ^ 2) * ( 60 ^ 2 ) * 24

	return( c( Tair=avg_temp, V=V, S=S, Day=mean( d$Day ), Month=mean( d$Month ), N=nrow( d ), flux=flux ) )
}

# ==============================================================================
# Main

if( !file.exists( OUTPUT_DIR ) ) {
	printlog( "Creating", OUTPUT_DIR )
	dir.create( OUTPUT_DIR )
}
if( !file.exists( LOG_DIR ) ) {
	printlog( "Creating", LOG_DIR )
	dir.create( LOG_DIR )
}

sink( paste0( LOG_DIR, SCRIPTNAME, ".txt" ), split=T )

printlog( "Welcome to", SCRIPTNAME )

loadlibs( c( "ggplot2", "reshape2", "plyr" ) )
theme_set( theme_bw() )

alldata <- data.frame()

for( fn in files ) {
	printlog( SEPARATOR )
	alldata <- rbind( alldata, read_egmfile( fn ) )
}

printlog( SEPARATOR )
printlog( "All done reading data." )
printdims( alldata )

printlog( "Merging respiration data with dry mass data..." )
# TODO

heights <- get_height()

printlog( "Computing fluxes..." )

# Compute flux per-plot with height correction.
flux <- function( d, h ) {
	d$Height <- sapply( d$Plot, function( x ) h[ which( x == h$Plot ),"Height" ] )
	flux <- ddply( d, .( filename, Plot ), .fun=compute_flux )
	flux
}

fluxes <- flux( alldata, heights )

print( summary( fluxes ) )

printlog( "Saving flux data..." )
savedata( alldata )
printlog( "Saving flux data..." )
savedata( fluxes )

printlog( "All done with", SCRIPTNAME )
#print( sessionInfo() )
sink()
