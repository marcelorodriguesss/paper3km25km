#Makefile rules include 
############################## Change Log ##################################
# 2.0.0
#
############################################################################


.SUFFIXES:
.SUFFIXES: .F90 .f90 .f .c .o .a

# f90 rule.

.f90.a:
	@echo ""
	$(F_COMMAND) $<
	$(ARCHIVE) $@ $(<F:.f90=.o)
	rm -f $(<F:.f90=.o)

# f77 rule.

.f.a:
	@echo ""
	$(F77_COMMAND) $<
	$(ARCHIVE) $@ $(<F:.f=.o)
	rm -f $(<F:.f=.o)

# F90 rule.

.F90.a:
	@echo ""
	$(F_COMMAND) -D$(CMACH) $<
	$(ARCHIVE) $@ $(<F:.F90=.o)
	rm -f $(<F:.F90=.o)

# c rule.

.c.a:
	@echo ""
	$(C_COMMAND) $<
	$(ARCHIVE) $@ $(<F:.c=.o)
	rm -f $(<F:.c=.o)
