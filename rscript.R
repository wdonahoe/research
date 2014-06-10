#!/usr/bin/Rscript

args = commandArgs(trailingOnly = TRUE)

build_data_frame <- function(files){
	df <- data.frame(sapply(files, scan))
	print(df)
}

build_data_frame(args)

##print(args)

##a = sample(0:1, 20, replace = TRUE)

##write(a, "out.txt", append = TRUE)












