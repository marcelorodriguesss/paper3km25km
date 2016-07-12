/*
!
! Copyright (C) 1991-2004  ; All Rights Reserved ; ATMET, LLC
! 
! This file is free software; you can redistribute it and/or modify it under the
! terms of the GNU General Public License as published by the Free Software 
! Foundation; either version 2 of the License, or (at your option) any later version.
! 
! This software is distributed in the hope that it will be useful, but WITHOUT ANY 
! WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License along with this 
! program; if not, write to the Free Software Foundation, Inc., 
! 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
!======================================================================================
!############################# Change Log ##################################
! 2.5.0
!###########################################################################
*/

#if defined(SUN) || defined(ALPHA) || defined(SGI) || defined(PC_LINUX1)
#define var_defaults_read var_defaults_read_
#define fgksinit fgksinit_
#define fgksclose fgksclose_
#define rams_anal_init rams_anal_init_
#define rams_get_idata rams_get_idata_
#define clearframe clearframe_
#define cread_rams cread_rams_
#define slab_stats slab_stats_
#define var_getcat var_getcat_
#define rams_get_fdata rams_get_fdata_
#define var_contours var_contours_
#define startmeta startmeta_
#define endmeta endmeta_
#define slab_coor slab_coor_
#define var_setcontours var_setcontours_
#define var_resetcontours var_resetcontours_
#define var_defcontours var_defcontours_
#define var_getparams var_getparams_
#define getmappos getmappos_
#define loc_slab loc_slab_
#define get_closez get_closez_
#define get_plevs get_plevs_
#define get_closep get_closep_
#define fegetenv fegetenv_
#endif
