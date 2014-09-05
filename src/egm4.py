#!/usr/bin/python
import os
import sys
import subprocess
import shutil
import string
import optparse
import datetime
import platform

SCRIPT_NAME = "egm4.R"
PLATFORM = platform.system() != "Windows"


def run(folder):
	""" Execute an R script on a series of text files.
	R script takes multiple files as arguments.

	Arguments:
	folder -- the name of the folder containing the text files (default)
	"""	

	if platform:
		try:
			subprocess.call(["./" + SCRIPT_NAME, folder])
		except OSError as e:
			print "OS error({0}): {1}".format(e.errno, e.strerror)
	else:
		try:
			subprocess.call(["Rscript", SCRIPT_NAME, folder])
		except OSError as e:
			print "OS error({0}): {1}".format(e.errno, e.strerror)


def main():
	if not PLATFORM:
		usage = "usage: %s foldername" % os.path.basename(sys.argv[0])
	else:
		usage = "usage: ./%s foldername" % os.path.basename(sys.argv[0])
	parser = optparse.OptionParser(usage = usage)
	options, args = parser.parse_args()

	if len(args) is not 1:
		parser.error("incorrect number of arguments.")
	else:
		run(args[0])
		

if __name__ == "__main__":
	main()

