FC=gfortran
FCFLAGS=-c -fcheck=bounds -O2 -mtune=native
LDFLAGS=

OBJECTS=modules.o evcalc.o inpsum.o ouset.o prise.o sigmas.o calc1.o evoutput.o  meset.o output.o reset.o soset.o calc2.o evset.o metext.o pitarea.o setup.o tempgrid.o coset.o iblval.o prime.o siggrid.o windgrid.o aermod.o 

all: $(OBJECTS)
	$(FC) $(LDFLAGS) $(OBJECTS) -o aermod

%.o: %.f
	$(FC) $(FCFLAGS) $< -o $@

clean:
	rm -rf *.o *.mod *.exe aermod
