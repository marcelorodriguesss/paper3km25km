#Makefile


# RAMS root directory.

RAMS_ROOT=/home/desenvolv/rams60-marcelo/rams60

# Versions.

RAMS_VERSION=6.0
UTILS_VERSION=2.4

# Source directories.

UTILS_LIB=$(RAMS_ROOT)/src/utils/$(UTILS_VERSION)/lib
UTILS_GDF=$(RAMS_ROOT)/src/utils/$(UTILS_VERSION)/gdf
UTILS_ENV=$(RAMS_ROOT)/src/utils/$(UTILS_VERSION)/env
NCARGD=$(RAMS_ROOT)/src/utils/$(UTILS_VERSION)/ncargd
UTILS_INCS=$(RAMS_ROOT)/src/utils/$(UTILS_VERSION)/include
UTILS_MODS=$(RAMS_ROOT)/src/utils/$(UTILS_VERSION)/lib/modules

MODEL=$(RAMS_ROOT)/src/rams/$(RAMS_VERSION)/src
