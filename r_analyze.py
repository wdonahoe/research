#!/usr/bin/python
import os
import sys
import subprocess
import shutil

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

	if not folder is None:
		if not script in os.listdir(folder):
			shutil.copy2(script, folder)
		os.chdir(folder)

	args = [file for file in os.listdir(os.getcwd()) if file.endswith('.txt') and not file is "out.txt"]
	args.sort(key = lambda x : os.path.getmtime(x)) ## Sort by last change.
	
	subprocess.check_call(["./" + script] + args, stderr = subprocess.STDOUT)
	print "Done."
	os.remove(script)

def main(argv):
	if len(argv) == 1:
		print_usage()
	elif len(argv) < 3:
		read_files(argv[1])
	else:
		read_files(argv[1], argv[2])

if __name__ == "__main__":
	main(sys.argv)

