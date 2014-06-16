#!/usr/bin/python
import os
import sys
import subprocess
import shutil
import string

def print_usage():
	message = "\nUsage: ./r_analyze.py <R Script> <folder>\n"
	print message

def read_files(script, folder = None):
	""" Execute an R script on a series of text files.
	R script takes multiple files as arguments.

	Arguments:
	script -- the name of the R script (with or without extension)
	folder -- the name of the folder containing the text files (default)
	"""	
	if not script.endswith(".R"):
		script += ".R"

	if folder is None:
		folder = os.getcwd()
		sp = string.split(folder,'/')
		folder = sp[len(sp) - 1]
	
	args = [file for file in os.listdir(folder) if file.endswith('.dat')]
	args.sort(key = lambda x : os.path.getctime(folder +"/" + x)) ## Sort by creation time.

	subprocess.call(["./" + script] + [folder + "/"] + args)

def main(argv):
	if len(argv) == 1:
		print_usage()
	elif len(argv) < 3:
		read_files(argv[1])
	else:
		read_files(argv[1], argv[2])

if __name__ == "__main__":
	main(sys.argv)

