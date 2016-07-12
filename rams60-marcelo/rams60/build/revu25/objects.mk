# Define main source.

MAIN_OBJ = ./revu_main.o
MAIN_SRC = $(REVU)/revu_main.f90

# Define objects.

OBJ = $(ARC)($(UTILS_MODS)/an_header.o) \
      $(ARC)($(POST_MODS)/hypparts.o) \
      \
      $(ARC)($(REVU)/interp.o) \
      $(ARC)($(REVU)/RAMS_grads.o) \
      $(ARC)($(REVU)/RAMS_grib.o) \
      $(ARC)($(REVU)/paramset.o) \
      $(ARC)($(REVU)/rbat.o) \
      $(ARC)($(REVU)/v5dout.o) \
      $(ARC)($(REVU)/getvar2xh.o) \
      \
      $(ARC)($(COMMON)/barbs.o) \
      $(ARC)($(COMMON)/contour.o) \
      $(ARC)($(COMMON)/defparse.o) \
      $(ARC)($(COMMON)/dumpout.o) \
      $(ARC)($(COMMON)/grabstat.o) \
      $(ARC)($(COMMON)/hvlib.o) \
      $(ARC)($(COMMON)/iplt.o) \
      $(ARC)($(COMMON)/landmarks.o) \
      $(ARC)($(COMMON)/medoc.o) \
      $(ARC)($(COMMON)/mkmap.o) \
      $(ARC)($(COMMON)/obsfill.o) \
      $(ARC)($(COMMON)/parts.o) \
      $(ARC)($(COMMON)/plevs.o) \
      $(ARC)($(COMMON)/plt.o) \
      $(ARC)($(COMMON)/rainit.o) \
      $(ARC)($(COMMON)/ramscode.o) \
      $(ARC)($(COMMON)/ramsgks.o) \
      $(ARC)($(COMMON)/rcolors.o) \
      $(ARC)($(COMMON)/rcomp.o) \
      $(ARC)($(COMMON)/revugrads.o) \
      $(ARC)($(COMMON)/rvec.o) \
      $(ARC)($(COMMON)/stats.o) \
      $(ARC)($(COMMON)/tiles.o) \
      $(ARC)($(COMMON)/vplt.o) \
      \
      $(ARC)($(MODEL)/rcio.o) \
      \
      $(ARC)($(REVU)/binio.o) \
      $(ARC)($(REVU)/v5d.o)
