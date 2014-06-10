#!/usr/bin/python
import os
import sys
import subprocess
import shutil

def read_files(script, folder = None):
	""" Execute an R script on a series of text files.

	Arguments:
	script -- the name of the R script (with or without extension)
	folder -- the name of the folder containing the text files (default)
	"""
	cwd_last = os.getcwd().split('/')
	if not script.endswith(".R"):
		script += ".R"

	if not folder is None:
		if not script in os.listdir(folder):
			shutil.copy2(script, folder)
		os.chdir(folder)
		
	for file in os.listdir(os.getcwd()):
		if file.endswith(".txt"):
			print "Analyzing " + file + "."
			subprocess.check_call(["./" + script, file], stderr = subprocess.STDOUT)
	print "Done."
	os.remove(script)

def main(argv):
	if len(argv) < 3:
		read_files(argv[1])
	else:
		read_files(argv[1], argv[2])

if __name__ == "__main__":
	main(sys.argv)

