#Makefile objects.mk

# Define main source.

MAIN = $(MODEL)/core/rammain.f90


# Define objects.

OBJ = $(ARC)($(UTILS_MODS)/an_header.o) \
      $(ARC)($(UTILS_LIB)/hdf5_utils.o) \
      \
      $(ARC)($(UTILS_MODS)/rconstants.o) \
      $(ARC)($(UTILS_GDF)/gdf_input.o) \
      \
      $(ARC)($(MODEL)/memory/grid_dims.o) \
      $(ARC)($(MODEL)/micro/micphys.o) \
      $(ARC)($(MODEL)/io/io_params.o) \
      $(ARC)($(MODEL)/core/ref_sounding.o) \
      $(ARC)($(MODEL)/mpi/node_mod.o) \
      $(ARC)($(MODEL)/mpi/rpara.o) \
      $(ARC)($(MODEL)/turb/ke_coms.o) \
      \
      $(ARC)($(MODEL)/memory/var_tables.o) \
      $(ARC)($(MODEL)/memory/mem_basic.o) \
      $(ARC)($(MODEL)/memory/mem_cuparm.o) \
      $(ARC)($(MODEL)/memory/mem_leaf.o) \
      $(ARC)($(MODEL)/memory/mem_micro.o) \
      $(ARC)($(MODEL)/memory/mem_radiate.o) \
      $(ARC)($(MODEL)/memory/mem_scalar.o) \
      $(ARC)($(MODEL)/memory/mem_scratch.o) \
      $(ARC)($(MODEL)/memory/mem_turb.o) \
      $(ARC)($(MODEL)/memory/mem_tend.o) \
      $(ARC)($(MODEL)/memory/mem_grid.o) \
      $(ARC)($(MODEL)/memory/mem_varinit.o) \
      $(ARC)($(MODEL)/memory/mem_nestb.o) \
      $(ARC)($(MODEL)/memory/mem_mksfc.o) \
      $(ARC)($(MODEL)/memory/mem_oda.o) \
      $(ARC)($(MODEL)/memory/mem_all.o) \
      $(ARC)($(MODEL)/memory/vtab_fill.o) \
      $(ARC)($(MODEL)/memory/alloc.o)   \
      $(ARC)($(MODEL)/memory/grid_struct.o) \
      \
      $(ARC)($(MODEL)/core/raco_adap.o) \
      $(ARC)($(MODEL)/core/radvc_adap.o) \
      $(ARC)($(MODEL)/core/rtimh.o) \
      $(ARC)($(MODEL)/core/rams_master.o) \
      $(ARC)($(MODEL)/core/rthrm.o) \
      $(ARC)($(MODEL)/core/model.o) \
      $(ARC)($(MODEL)/core/coriolis.o) \
      $(ARC)($(MODEL)/core/raco.o) \
      $(ARC)($(MODEL)/core/radvc.o) \
      $(ARC)($(MODEL)/core/rtimi.o) \
      $(ARC)($(MODEL)/core/modsched.o) \
      \
      $(ARC)($(MODEL)/init/rhhi.o)  \
      $(ARC)($(MODEL)/init/rinit.o)  \
      $(ARC)($(MODEL)/init/rams_grid.o) \
      $(ARC)($(MODEL)/init/rdint.o)  \
      $(ARC)($(MODEL)/init/gridset.o)  \
      $(ARC)($(MODEL)/init/adap_init.o)  \
      $(ARC)($(MODEL)/init/lllc_utils.o)  \
      \
      $(ARC)($(MODEL)/bc/cyclic_mod.o) \
      $(ARC)($(MODEL)/bc/rbnd.o) \
      $(ARC)($(MODEL)/bc/rbnd_adap.o) \
      \
      $(ARC)($(MODEL)/surface/sfc_driver.o) \
      $(ARC)($(MODEL)/surface/leaf_coms.o) \
      $(ARC)($(MODEL)/surface/leaf3.o) \
      $(ARC)($(MODEL)/surface/leaf3_init.o) \
      $(ARC)($(MODEL)/surface/leaf3_prep.o) \
      $(ARC)($(MODEL)/surface/leaf3_canopy.o) \
      $(ARC)($(MODEL)/surface/leaf3_slayer.o) \
      $(ARC)($(MODEL)/surface/leaf3_soilveg.o) \
      $(ARC)($(MODEL)/surface/leaf3_hyd.o) \
      $(ARC)($(MODEL)/surface/urban_canopy.o) \
      $(ARC)($(MODEL)/surface/ruser.o) \
      \
      $(ARC)($(MODEL)/mpi/mpass_full.o)      \
      $(ARC)($(MODEL)/mpi/mpass_dtl.o) \
      $(ARC)($(MODEL)/mpi/mpass_feed.o) \
      $(ARC)($(MODEL)/mpi/mpass_init.o) \
      $(ARC)($(MODEL)/mpi/mpass_lbc.o)  \
      $(ARC)($(MODEL)/mpi/mpass_nest.o) \
      $(ARC)($(MODEL)/mpi/mpass_oda.o) \
      $(ARC)($(MODEL)/mpi/mpass_st.o) \
      $(ARC)($(MODEL)/mpi/mpass_cyclic.o) \
      $(ARC)($(MODEL)/mpi/rnest_par.o) \
      $(ARC)($(MODEL)/mpi/para_init.o) \
      $(ARC)($(MODEL)/mpi/par_decomp.o) \
      $(ARC)($(MODEL)/mpi/rnode.o) \
      \
      $(ARC)($(MODEL)/nesting/nest_drivers.o) \
      $(ARC)($(MODEL)/nesting/nest_filldens.o) \
      $(ARC)($(MODEL)/nesting/nest_intrp.o) \
      $(ARC)($(MODEL)/nesting/nest_move.o) \
      $(ARC)($(MODEL)/nesting/nest_feed.o) \
      $(ARC)($(MODEL)/nesting/hemi2.o) \
      \
      $(ARC)($(MODEL)/radiate/rrad3.o) \
      $(ARC)($(MODEL)/radiate/rad_driv.o) \
      $(ARC)($(MODEL)/radiate/rad_mclat.o) \
      $(ARC)($(MODEL)/radiate/rad_ccmp.o) \
      $(ARC)($(MODEL)/radiate/rad_stable.o) \
      $(ARC)($(MODEL)/radiate/rrad2.o) \
      \
      $(ARC)($(MODEL)/micro/mic_init.o) \
      $(ARC)($(MODEL)/micro/mic_coll.o) \
      $(ARC)($(MODEL)/micro/mic_driv.o) \
      $(ARC)($(MODEL)/micro/mic_misc.o) \
      $(ARC)($(MODEL)/micro/mic_vap.o) \
      $(ARC)($(MODEL)/micro/mic_nuc.o)  \
      $(ARC)($(MODEL)/micro/mic_tabs.o) \
      $(ARC)($(MODEL)/micro/mic_gamma.o) \
      \
      $(ARC)($(MODEL)/io/anal_extra.o) \
      $(ARC)($(MODEL)/io/history_start.o) \
      $(ARC)($(MODEL)/io/anal_write.o) \
      $(ARC)($(MODEL)/io/rcio.o) \
      $(ARC)($(MODEL)/io/inithis.o)  \
      $(ARC)($(MODEL)/io/recycle.o)  \
      $(ARC)($(MODEL)/io/opspec.o) \
      $(ARC)($(MODEL)/io/rname.o) \
      $(ARC)($(MODEL)/io/ranlavg.o) \
      $(ARC)($(MODEL)/io/rprnt.o) \
      \
      $(ARC)($(MODEL)/mksfc/sst_read.o) \
      $(ARC)($(MODEL)/mksfc/ndvi_read.o) \
      $(ARC)($(MODEL)/mksfc/mksfc_driver.o) \
      $(ARC)($(MODEL)/mksfc/mksfc_sfc.o) \
      $(ARC)($(MODEL)/mksfc/mksfc_top.o) \
      $(ARC)($(MODEL)/mksfc/mksfc_sst.o) \
      $(ARC)($(MODEL)/mksfc/mksfc_ndvi.o) \
      $(ARC)($(MODEL)/mksfc/geodat.o) \
      $(ARC)($(MODEL)/mksfc/landuse_input.o) \
      $(ARC)($(MODEL)/mksfc/nest_geosst.o) \
      \
      $(ARC)($(MODEL)/fdda/oda_read.o) \
      $(ARC)($(MODEL)/fdda/oda_krig.o) \
      $(ARC)($(MODEL)/fdda/oda_nudge.o) \
      $(ARC)($(MODEL)/fdda/oda_proc_obs.o) \
      $(ARC)($(MODEL)/fdda/oda_sta_count.o) \
      $(ARC)($(MODEL)/fdda/oda_sta_input.o) \
      $(ARC)($(MODEL)/fdda/varf_read.o) \
      $(ARC)($(MODEL)/fdda/varf_update.o) \
      $(ARC)($(MODEL)/fdda/nud_read.o) \
      $(ARC)($(MODEL)/fdda/nud_update.o) \
      $(ARC)($(MODEL)/fdda/cond_read.o) \
      $(ARC)($(MODEL)/fdda/cond_update.o) \
      $(ARC)($(MODEL)/fdda/nud_analysis.o) \
      \
      $(ARC)($(MODEL)/turb/turb_k.o) \
      $(ARC)($(MODEL)/turb/turb_ke.o) \
      $(ARC)($(MODEL)/turb/turb_diff.o) \
      $(ARC)($(MODEL)/turb/turb_k_adap.o) \
      $(ARC)($(MODEL)/turb/turb_diff_adap.o) \
      $(ARC)($(MODEL)/turb/rgrad.o) \
      \
      $(ARC)($(MODEL)/isan/isan_coms.o) \
      $(ARC)($(MODEL)/isan/aobj.o) \
      $(ARC)($(MODEL)/isan/asgen.o) \
      $(ARC)($(MODEL)/isan/asti2.o) \
      $(ARC)($(MODEL)/isan/asti.o) \
      $(ARC)($(MODEL)/isan/astp.o) \
      $(ARC)($(MODEL)/isan/avarf.o) \
      $(ARC)($(MODEL)/isan/file_inv.o) \
      $(ARC)($(MODEL)/isan/first_rams.o) \
      $(ARC)($(MODEL)/isan/isan_name.o) \
      $(ARC)($(MODEL)/isan/isan_io.o) \
      $(ARC)($(MODEL)/isan/refstate.o) \
      $(ARC)($(MODEL)/isan/v_interps.o) \
      $(ARC)($(MODEL)/isan/write_varf.o) \
      \
      $(ARC)($(MODEL)/cuparm/conv_coms.o) \
      $(ARC)($(MODEL)/cuparm/kf_cuparm.o) \
      $(ARC)($(MODEL)/cuparm/kf_rconv.o) \
      $(ARC)($(MODEL)/cuparm/kf_driver.o) \
      $(ARC)($(MODEL)/cuparm/rconv.o) \
      $(ARC)($(MODEL)/cuparm/cu_read.o)
