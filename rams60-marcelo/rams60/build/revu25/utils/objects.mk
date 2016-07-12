OBJ = $(ARC)($(UTILS_MODS)/an_header.o)  \
      \
      $(ARC)($(UTILS_LIB)/hdf5_f2c.o) \
      $(ARC)($(UTILS_LIB)/hdf5_utils.o) \
      \
      $(ARC)($(UTILS_LIB)/charutils.o) \
      $(ARC)($(UTILS_LIB)/dateutils.o) \
      $(ARC)($(UTILS_LIB)/error_mess.o) \
      $(ARC)($(UTILS_LIB)/getvar.o) \
      $(ARC)($(UTILS_LIB)/interp_lib.o) \
      $(ARC)($(UTILS_LIB)/map_proj.o) \
      $(ARC)($(UTILS_LIB)/numutils.o) \
      $(ARC)($(UTILS_LIB)/polarst.o) \
      $(ARC)($(UTILS_LIB)/rnamel.o) \
      $(ARC)($(UTILS_LIB)/therm_lib.o) \
      $(ARC)($(UTILS_LIB)/utils_f.o) \
      $(ARC)($(UTILS_LIB)/vformat.o) \
      \
      $(ARC)($(UTILS_LIB)/filelist.o) \
      $(ARC)($(UTILS_LIB)/rsys.o) \
      \
      $(ARC)($(UTILS_GDF)/gdf_input.o) \
      $(ARC)($(UTILS_GDF)/gdf_read_sfc.o) \
      $(ARC)($(UTILS_GDF)/gdf_read_upa.o) \
      $(ARC)($(UTILS_GDF)/gdf_vars.o) \
      \
      $(ARC)($(UTILS_LIB)/parlib.o) \
      $(ARC)($(UTILS_LIB)/tmpname.o) \
      $(ARC)($(UTILS_LIB)/utils_c.o) \
      $(ARC)($(EFF)/eenviron.o)
      
      
      #$(ARC)($(UTILS_LIB)/hdf5_rw.o) \
      
      
      #$(ARC)($(UTILS_GDF)/gdf_write.o) \
      #$(ARC)($(UTILS_GDF)/gdf_write_sfc.o) \
      #$(ARC)($(UTILS_GDF)/gdf_write_upa.o) \
