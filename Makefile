#@+leo-ver=4-thin
#@+node:gcross.20091211100630.1234:@thin Makefile
#@@language Makefile
#@@tabwidth 4

all: \
	programs/plot-gadget-state-difference \
	programs/plot-gadget-energy-gap \

LIBS = -larpack -llapack -lblas -lgfortran

HFLAGS = -O2 -fvia-C -optc=-O3 -isources

GHCMAKE = ghc --make ${HFLAGS} ${LIBS}

programs/%: sources/%.hs Makefile sources/VMPSDatabase.hs
	${GHCMAKE} $< -o $@
#@-node:gcross.20091211100630.1234:@thin Makefile
#@-leo
