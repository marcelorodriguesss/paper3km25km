#Makefile Make.revu
############################## Change Log ##################################
# 2.3.0.1
#
# 000910 MJB Make.revu ##
#            Removed objects error_mess.o and hutil.o (duplicated in
#            UTILS). ##
# 000908 MJB Make.revu ##
#            New methodology in rules and targets.
#            Added dependencies on include and use files. ##
# 000826 MJB Make.revu ##
#            Make file environment variables now defined in "include.mk".
#            Add check and lib targets. ##
#
############################################################################

# Include definitions (if this does not work, try using gnu make or copy the
# contents of the file include.mk here).

include ./paths.mk
include ./include.mk.$(OPT)


# Compiler commands.

INCLUDES  = -I$(UTILS_INCS) -I$(POST_INCS) -I$(MODEL_INCS) -I./utils
F_COMMAND = $(F_COMP) -c $(F_OPTS) $(INCLUDES)
C_COMMAND = $(C_COMP) -c $(C_OPTS) -D$(CMACH) $(INCLUDES)

include ../rules.mk

# Define archive and executable names.

BASE=revu
EXE=$(BASE)-$(REVU_VERSION)
ARC=$(BASE)-$(REVU_VERSION).a


include ./objects.mk
      
# RAMS libraries.

LIBUTILS=./utils/libutils-$(UTILS_VERSION)-$(OPT).a


# Define targets.

all: $(EXE)

$(EXE): $(LIBUTILS) $(ARC) $(MAIN_SRC) FORCE
	@echo ""
	$(LOADER) -o $(EXE) $(MAIN_OBJ) $(LOADER_OPTS) $(ARC) \
	$(LIBUTILS) $(LIBNCARG) $(LIBS) $(HDF5_LIBS)
	rm -f $(MAIN_OBJ)

$(MAIN_SRC): FORCE
	@echo ""
	cp -f $@ $(@F:.f90=.f)
	$(F_COMMAND) $(@F:.f90=.f)

$(ARC): $(OBJ)

$(LIBUTILS): FORCE
	(cd ./utils ; $(MAKE) -f Make.utils.$(OPT) )

FORCE:

check: FORCE
	@echo ""
	check

install:
	@echo ""
	ln -fs `pwd`/$(EXE) ../run/$(BASE)
	ln -fs `pwd`/$(EXE) ../test/$(BASE)
	@echo ""

clean:
	@echo ""
	(cd ./utils ; $(MAKE) -f Make.utils.$(OPT) clean)
	rm -f $(ARC) $(EXE) $(BASE) *.o *.mod *.f *.stb
	@echo ""

# Include dependencies (if this does not work, try using gnu make or copy the
# contents of the file dep_revu here).

#include dep_revu.mk
