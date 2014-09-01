#!/usr/bin/env Rscript

# R script to process EGM-4 data
# BBL April 2014

# Support functions and common definitions

args = commandArgs( trailingOnly = TRUE )

# Constants
SCRIPTNAME		<- "egm4.R"
OUTPUT_DIR		<- "out"
LOG_DIR			<- "logs"
SEPARATOR		<- "-------------------"
MEAS_INTERVAL	<- 10
HEIGHT_OPTIONS 	<- "heights.txt"
SEP <- ifelse(Sys.info()['sysname'] != "Windows","/","\\")
INPUT_DIR		<- args[1]
DATE_FORMAT		<- "%B%d%Y"
FILE_FORMAT 	<- ".+\\.dat"

get_files <- function(){

	files <- list.files( INPUT_DIR, FILE_FORMAT )

	cat( "Reading the following files:\n" )
	for ( file in files )
		cat(file, sep = "\n")

	return( unlist( files ) )

}
	

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
	fn <- paste0( OUTPUT_DIR, SEP, format( Sys.time(),"%d%B%Y_%H%M%S" ),"_fluxes",extension )
	printlog( "Saving", fn )
	write.csv( df, fn, row.names=FALSE )
} # savedata

# Put height data in a frame.
get_height <- function( ) {
	heights <- paste0( INPUT_DIR, SEP, HEIGHT_OPTIONS )
	stopifnot( file.exists( heights ) )
	h <- read.table( heights, comment.char=";",sep="\t" )

	names( h ) <- c("Plot", "Height")
	arrange(h, Plot)

	return( h )
} #get_height

# Convert a filename containing a date to a Date object.
format_date <- function( fn ) {
	date <- unlist( strsplit( fn, c( "P","." ), fixed=TRUE ) )[ 1 ]

	return( as.Date( date, DATE_FORMAT ) )
} #format_date

# -----------------------------------------------------------------------------
# Load requested libraries
loadlibs <- function( liblist ) {
	printlog( "Loading libraries..." )
	loadedlibs <- vector()
	for( lib in liblist ) {
		printlog( "Loading", lib )
		loadedlibs[ lib ] <- require( lib, character.only=TRUE )
		if( !loadedlibs[ lib ] )
			warning( "this package is not installed!" )
	}
	invisible( loadedlibs )
} # loadlibs

quality_control <- function( d ) {

	mods <- dlply( d, .( Plot ), lm, formula = CO2_Ref ~ Sec )
	r2 <- ldply( mods, .fun=function( x ){ round( summary( x )$r.squared, 2 ) } )
	names( r2 ) <- c( "Plot", "R2" )
	r2 <- r2[ order( r2$R2 ), ]
	print( r2 )
}

# -----------------------------------------------------------------------------
# read a process a single EGM4 output file, returning data frame
read_egmfile <- function( fn ) {
	fqfn <- paste0( INPUT_DIR, SEP, fn )
	printlog( "Reading", fqfn )
	stopifnot( file.exists( fqfn ) )
	d <- read.table( fqfn, comment.char=";", sep="\t", na.strings="")
	d <- d[ 1:19 ]
	printdims( d )
	names( d ) <- c( "Plot", "RecNo", "Day", "Month", "Hour", "Min", "CO2_Ref", "mb_Ref",
		 "mbR_Temp", "Input_A", "Input_B", "Input_C", "Input_D", "Input_E", "Input_F", 
		 "Input_G", "Input_H", "ATMP", "Probe Type" )
	d$filename <- fn
	printlog( "Adding seconds (interval =", MEAS_INTERVAL, ")" )
	d <- ddply( d, .( Plot ), mutate, Sec=seq( from=0, length.out=length( Plot ), by=MEAS_INTERVAL ) )
	
	# QC
	printlog( "Computing CO2~Time R2 values for quality control..." )
	quality_control( d ) 

	return( d )
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
	S 			<- (pi * ring_r ^ 2) * 1.0e-3			# note m2, not cm2!
	sleeve_vol 	<- sleeve_ht * S 						# cm3
	V			<- ( egm4_vol + sleeve_vol ) * 1.0e-6	# m3, not cm3!
	R 			<- 8.3145e-3							# m-3 kPa mol-1 K-1
	Kelvin		<- 273.15								# C to K conversion
	avg_temp 	<- mean( d$Input_C )					# assumes EGM temperature probe connected
	Pa 			<- 101									# kPa

	flux <- Resp_raw * ( V / S ) * Pa / ( R * avg_temp )

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

sink( paste0( LOG_DIR, SCRIPTNAME, ".txt" ), split=TRUE )

printlog( "Welcome to", SCRIPTNAME )

loadlibs( c( "plyr" ) )

alldata <- data.frame()

for( fn in get_files() ) {
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
	return( flux )
}

fluxes <- flux( alldata, heights )
fluxes <- fluxes[ order( sapply( fluxes$filename, function( x ) format_date( x ) ) ), ] # sort by date

print( summary( fluxes ) )

printlog( "Saving flux data..." )
savedata( alldata )
printlog( "Saving flux data..." )
savedata( fluxes )

printlog( "All done with", SCRIPTNAME )
#print( sessionInfo() )
sink()
