import sys

def usage():
    print """
    python infile outfile
    """

if len(sys.argv)!=3:
    usage()
    sys.exit(2)

try:
    infile = file(sys.argv[1],"r")
    outfile = file(sys.argv[2],"w")
except IOError:
    sys.stderr.write("ERROR: Cannot read/write inputfile %s.\n" % arg)
    sys.exit(1)

outfile.write(infile.read().replace("\\","\\\\"))