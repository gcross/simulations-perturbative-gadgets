#@+leo-ver=4-thin
#@+node:gcross.20091211100630.1234:@thin Makefile
#@@language Makefile
#@@tabwidth 4

all: \
	programs/plot-gadget-state-difference \
	programs/plot-gadget-energy-gap \
	programs/simulate-gadget \
	programs/sweep-bitslayer-gadget \

LIBS = 

HFLAGS = -O2 -fvia-C -optc=-O3 -isources

GHCMAKE = ghc --make ${HFLAGS} ${LIBS}

programs/%: sources/%.hs sources/Models.hs Makefile /usr/local/lib/VMPS-0.1/ghc-6.10.4/libvmps.a
	${GHCMAKE} $< -o $@

clean:
	rm -f programs/* sources/*.o sources/*.hi
#@-node:gcross.20091211100630.1234:@thin Makefile
#@-leo
