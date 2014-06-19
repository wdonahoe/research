#!/usr/bin/python
import os
import sys
import subprocess
import shutil
import string
import optparse

def print_usage():
	message = "\nUsage: ./r_analyze.py <R Script> <height_file> [<folder>]\n"
	print message

def read_files(script, height, folder = None):
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

	args.insert(0, height) if height.endswith(".txt") else args.insert(0, height + ".txt")

	try:
		subprocess.call(["./" + script] + [folder + "/"] + args)
	except OSError as e:
		print "OS error({0}): {1}".format(e.errno, e.strerror)


def main():
	parser = optparse.OptionParser()

	options, args = parser.parse_args()

	if (len(args) == 0):
		print_usage()
	elif (len(args) == 2):
		read_files(args[0], args[1])
	elif (len(args) == 3):
		read_files(args[0], args[1], args[2])

if __name__ == "__main__":
	main()

