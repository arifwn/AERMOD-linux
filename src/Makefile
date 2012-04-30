FC=gfortran
FCFLAGS=-c -fcheck=bounds -O2 -mtune=native
LDFLAGS=

OBJECTS=modules.o aermod.o setup.o coset.o soset.o reset.o meset.o ouset.o inpsum.o metext.o iblval.o siggrid.o tempgrid.o windgrid.o calc1.o calc2.o prise.o prime.o sigmas.o pitarea.o output.o evset.o evcalc.o evoutput.o

all: $(OBJECTS)
	$(FC) $(LDFLAGS) $(OBJECTS) -o aermod

%.o: %.f
	$(FC) $(FCFLAGS) $< -o $@

clean:
	rm -rf *.o *.mod *.exe aermod
