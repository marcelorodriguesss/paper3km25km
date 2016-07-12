#Makefile 

#ifndef OPT
OPT=opt
#endif


# Include definitions 

include ./paths.mk
include ./include.mk.$(OPT)

# Compiler commands.

INCLUDES  = -I$(UTILS_INCS) -I./utils
F_COMMAND = $(F_COMP) -c $(F_OPTS) $(INCLUDES)
C_COMMAND = $(C_COMP) -c $(C_OPTS) $(INCLUDES)

include ../rules.mk

# Define archive and executable names

BASE=rams
EXE=$(BASE)-$(RAMS_VERSION)-$(OPT)
ARC=$(BASE)-$(RAMS_VERSION)-$(OPT).a


include objects.mk


# Define targets.

all: $(EXE)

$(EXE): $(LIBUTILS) $(ARC) $(MAIN) FORCE
	@echo ""
	$(LOADER) -o $(EXE) rammain.o $(LOADER_OPTS) $(ARC) \
	$(LIBUTILS) $(LIBNCARG) $(PAR_LIBS) $(LIBS) $(HDF5_LIBS)
	rm -f $(MAIN:.f90=.f) *.o
	@echo ""
	@echo Finished building === $(EXE)
	@echo ""

$(MAIN): FORCE
	@echo ""
	$(F_COMMAND) $@

$(ARC): $(OBJ)

$(LIBUTILS): FORCE
	(cd ./utils ; $(MAKE) -f Make.utils.$(OPT) )

FORCE:

install:
	@echo ""
	ln -fs `pwd`/$(EXE) ../run/$(BASE)
	ln -fs `pwd`/$(EXE) ../test/$(BASE)
	@echo ""

clean:
	@echo ""
	(cd ./utils ; $(MAKE) -f Make.utils.$(OPT) clean)
	rm -f $(ARC) $(EXE) *.o *.mod *.f *.stb
	@echo ""

# Include dependencies (if this does not work, try using gnu make or copy the
# contents of the file dep_rams here).

#include dep_rams.mk
