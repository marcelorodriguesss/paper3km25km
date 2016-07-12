#Makefile 


# RAMS root directory.

RAMS_ROOT=/home/desenvolv/rams60-marcelo/rams60

# Versions.

RAMS_VERSION=5.02
REVU_VERSION=2.5
UTILS_VERSION=2.2
HYPACT_VERSION=1.1.0

# Source directories.

REVU=$(RAMS_ROOT)/src/post/$(REVU_VERSION)/revu
COMMON=$(RAMS_ROOT)/src/post/$(REVU_VERSION)/common
POST_INCS=$(RAMS_ROOT)/src/post/$(REVU_VERSION)/include
POST_MODS=$(RAMS_ROOT)/src/post/$(REVU_VERSION)/common/modules

UTILS_LIB=$(RAMS_ROOT)/src/utils/$(UTILS_VERSION)/lib
EFF=$(RAMS_ROOT)/src/utils/$(UTILS_VERSION)/env
NCARGD=$(RAMS_ROOT)/src/utils/$(UTILS_VERSION)/ncargd
UTILS_GDF=$(RAMS_ROOT)/src/utils/$(UTILS_VERSION)/gdf
UTILS_INCS=$(RAMS_ROOT)/src/utils/$(UTILS_VERSION)/include
UTILS_MODS=$(RAMS_ROOT)/src/utils/$(UTILS_VERSION)/lib/modules

MODEL_LIB=$(RAMS_ROOT)/src/rams/$(RAMS_VERSION)/lib
MODEL=$(RAMS_ROOT)/src/rams/$(RAMS_VERSION)/model
MODEL_MODS=$(RAMS_ROOT)/src/rams/$(RAMS_VERSION)/modules
ISAN=$(RAMS_ROOT)/src/rams/$(RAMS_VERSION)/isan
ISAN_MODS=$(RAMS_ROOT)/src/rams/$(RAMS_VERSION)/isan/modules
MODEL_INCS=$(RAMS_ROOT)/src/rams/$(RAMS_VERSION)/include

HYPACT=$(RAMS_ROOT)/src/hypact/$(HYPACT_VERSION)/model
HYPACT_INCS=$(RAMS_ROOT)/src/hypact/$(HYPACT_VERSION)/include
HYPACT_MODS=$(RAMS_ROOT)/src/hypact/$(HYPACT_VERSION)/modules
