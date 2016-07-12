!
! Copyright (C) 1991-2004  ; All Rights Reserved ; Colorado State University
! Colorado State University Research Foundation ; ATMET, LLC
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
! 5.0.0
!
!###########################################################################

subroutine toptnest(ngra,ngrb)

use mem_mksfc
use mem_grid
use io_params

implicit none

integer :: ngra,ngrb 

integer :: ifm,icm,ipat,i,j,k,indfm,ivtime,nc1,mynum

do ifm = ngra,ngrb
   icm = nxtnest(ifm)
! Initialize TOPOGRAPHY in toptinit.

   call toptinit(nnxp(ifm),nnyp(ifm),ifm  &
      ,sfcfile_p(ifm)%topt(1,1),sfcfile_p(ifm)%topzo(1,1))

   if (icm .ge. 1 .and. itoptflg(ifm) .eq. 0) then
      ! Interpolate TOPO from coarser grid:
      call fillscr(1,nxpmax,nypmax,1,nnxp(icm),nnyp(icm),1,1  &
         ,scr1,sfcfile_p(icm)%topt(1,1))
      call eintp(scr1,scr2,1,nxpmax,nypmax  &
         ,1,nnxp(ifm),nnyp(ifm),ifm,2,'t',0,0)
      call fillvar(1,nxpmax,nypmax,1,nnxp(ifm),nnyp(ifm),1,1  &
         ,scr2,sfcfile_p(ifm)%topt(1,1))

      ! Interpolate TOPO ZO from coarser grid:
      call fillscr(1,nxpmax,nypmax,1,nnxp(icm),nnyp(icm),1,1  &
               ,scr1,sfcfile_p(icm)%topzo(1,1))
      call eintp(scr1,scr2,1,nxpmax,nypmax  &
               ,1,nnxp(ifm),nnyp(ifm),ifm,2,'t',0,0)
      call fillvar(1,nxpmax,nypmax,1,nnxp(ifm),nnyp(ifm),1,1  &
               ,scr2,sfcfile_p(ifm)%topzo(1,1))

   elseif (itoptflg(ifm) .eq. 1) then

      ! Interpolate TOPO from standard dataset:
      call geodat(nnxp(ifm),nnyp(ifm),sfcfile_p(ifm)%topt(1,1)  &
                 ,itoptfn(ifm),itoptfn(ifm),vt2da,vt2db,ifm,'TOP')
      ! Interpolate TOPO ZO from standard dataset:
      call geodat(nnxp(ifm),nnyp(ifm),sfcfile_p(ifm)%topzo(1,1)  &
               ,itoptfn(ifm),itoptfn(ifm),vt2da,vt2db,ifm,'ZOT')

   endif
   
! If desired, override current values of TOPOGRAPHY in ruser.f subroutine.

   call toptinit_user(nnxp(ifm),nnyp(ifm),ifm  &
         ,sfcfile_p(ifm)%topt(1,1) ,sfcfile_p(ifm)%topzo(1,1))

enddo

if (ngra .eq. ngrb) return

! In case topography data have been independently reassigned on any grid,
! average fine mesh topography sequentially to the coarser grids.

do ifm = ngrb,ngra,-1
   if (nxtnest(ifm) .gt. ngridsh .and. ifm .ge. 2) then
      icm = nxtnest(ifm)

      call fdback(sfcfile_p(icm)%topt(1,1),sfcfile_p(ifm)%topt(1,1)  &
         ,vt2da,scr2,1,nnxp(icm),nnyp(icm)  &
         ,1,nnxp(ifm),nnyp(ifm),ifm,'terr',vt2db)

   endif
enddo

! In case terrain heights have been independently reassigned on
! any grid, interpolate coarse grid terrain heights to a temporary
! fine mesh array.  Fill the fine mesh boundary terrain heights
! from the temporary array.

do ifm = ngra,ngrb
   icm = nxtnest(ifm)
   if (icm .ge. 1) then
      call fillscr(1,nxpmax,nypmax,1,nnxp(icm),nnyp(icm),1,1  &
         ,scr1,sfcfile_p(icm)%topt(1,1))

      call eintp(scr1,scr2,1,nxpmax,nypmax  &
         ,1,nnxp(ifm),nnyp(ifm),ifm,2,'t',0,0)
      call fillvar(1,nxpmax,nypmax,1,nnxp(ifm),nnyp(ifm),1,1  &
         ,scr2,scr1)

      nc1 = jdim * (nstraty(ifm) + 1)
      call ae2(nnxp(ifm),nnyp(ifm),2+nstratx(ifm)  &
         ,nnxp(ifm)-1-nstratx(ifm),1+nc1,nnyp(ifm)-nc1  &
         ,scr1,sfcfile_p(ifm)%topt(1,1))
      call ae1(nnxp(ifm)*nnyp(ifm),sfcfile_p(ifm)%topt(1,1),scr1)

   endif
enddo
return
end

!*************************************************************************

subroutine geonest_file(ifm)

use mem_mksfc
use mem_grid
use io_params
use mem_leaf

implicit none

integer :: ifm 

integer :: icm,ipat,i,j,k,indfm,ivtime,nc1,mynum

icm = nxtnest(ifm)

! Initialize PATCH AREA, LANDUSE CLASS, and SOIL TEXTURAL CLASS
! in subroutine sfcinit.

call sfcinit_file(nnxp(ifm),nnyp(ifm),nzg,npatch,ifm      &
   ,sfcfile_p(ifm)%patch_area(1,1,1)   &
   ,sfcfile_p(ifm)%leaf_class(1,1,1)   &
   ,sfcfile_p(ifm)%soil_text(1,1,1,1))
     

print*, ' '
print*,'====================================================='
print*,'Starting landuse data input on grid ',ifm
print*,'====================================================='

if (icm .ge. 1 .and. ivegtflg(ifm) .eq. 0) then

! Assign PATCH AREAS and PATCH CLASSES from coarser grid:

   do ipat = 1,npatch
      do j = 1,nnyp(ifm)
         do i = 1,nnxp(ifm)

            sfcfile_p(ifm)%patch_area(i,j,ipat) =  &
               sfcfile_p(icm)%patch_area(ipm(i,ifm),jpm(j,ifm),ipat)
            sfcfile_p(ifm)%leaf_class(i,j,ipat) =  &
               sfcfile_p(icm)%leaf_class(ipm(i,ifm),jpm(j,ifm),ipat)
                        
         enddo
      enddo
   enddo
   

elseif (ivegtflg(ifm) .eq. 1) then

! Assign PATCH AREAS and PATCH CLASSES from standard dataset:

   call landuse_opqr(nnxp(ifm),nnyp(ifm),nzg,npatch,nvegpat  &
      ,ivegtflg(ifm),ivegtfn(ifm),isoilflg(ifm),isoilfn(ifm) &
      ,ndviflg(ifm),ndvifn(ifm),vndvifil(1,ifm)  &
      ,'veg',platn(ifm),plonn(ifm)        &
      ,sfcfile_p(ifm)%soil_text(1,1,1,1)  &
      ,sfcfile_p(ifm)%patch_area(1,1,1)   &
      ,sfcfile_p(ifm)%leaf_class(1,1,1)   &
      ,sfcfile_p(ifm)%veg_ndvif(1,1,1))
   

endif

if (icm .ge. 1 .and. isoilflg(ifm) .eq. 0) then

! Assign SOIL TEXTURE CLASS from coarser grid

   do ipat = 2,npatch
      do k = 1,nzg
         do j = 1,nnyp(ifm)
            do i = 1,nnxp(ifm)
               sfcfile_p(ifm)%soil_text(k,i,j,ipat) =  &
               sfcfile_p(icm)%soil_text(k,ipm(i,ifm),jpm(j,ifm),ipat)
            enddo
         enddo
      enddo
   enddo

elseif (isoilflg(ifm) .eq. 1) then

! Assign SOIL TEXTURE CLASS from standard dataset:

   call landuse_opqr(nnxp(ifm),nnyp(ifm),nzg,npatch,nvegpat  &
      ,ivegtflg(ifm),ivegtfn(ifm),isoilflg(ifm),isoilfn(ifm) &
      ,ndviflg(ifm),ndvifn(ifm),vndvifil(1,ifm)  &
      ,'soil',platn(ifm),plonn(ifm)        &
      ,sfcfile_p(ifm)%soil_text(1,1,1,1)  &
      ,sfcfile_p(ifm)%patch_area(1,1,1)   &
      ,sfcfile_p(ifm)%leaf_class(1,1,1)   &
      ,sfcfile_p(ifm)%veg_ndvif(1,1,1))

endif

! If desired, override current values of PATCH AREA, PATCH CLASS, 
! LEAF-2 VEGETATION CLASS, SOIL TEXTURAL CLASS, and/or
! NDVI in ruser.f subroutines.

call sfcinit_file_user(nnxp(ifm),nnyp(ifm),nzg,npatch,ifm &
   ,sfcfile_p(ifm)%patch_area  (1,1,1)    &
   ,sfcfile_p(ifm)%leaf_class (1,1,1)     &
   ,sfcfile_p(ifm)%soil_text   (1,1,1,1) )

! As a final initialization step, eliminate any land patch area that is less 
! than 1% of the total grid cell area.  Set its area to zero, and compensate
! by enlarging areas of remaining patches.

call patch_minsize(nnxp(ifm),nnyp(ifm),npatch  &
   ,sfcfile_p(ifm)%patch_area(1,1,1))


return
end

!*************************************************************************

subroutine geonest_nofile(ngra,ngrb)

use mem_leaf
use mem_basic
use mem_scratch
use mem_grid
use io_params

implicit none

integer :: ngra,ngrb

integer :: isiz,ifm,icm,ipat,i,j,k,indfm,ivtime,nc1,mynum,ic,jc

! Initialization/interpolation of leaf-2 variables for which standard RAMS
! datasets never exist.

isiz = maxnxp * maxnyp

do ifm = ngra,ngrb
   icm = nxtnest(ifm)

! First, fill NOFILE LEAF-2 variables with default values in SFCINIT.

   call sfcinit_nofile(nnzp(ifm),nnxp(ifm),nnyp(ifm),nzg,nzs    &
      ,npatch,ifm                                               &
      ,basic_g(ifm)%theta          (1,1,1)    &
      ,basic_g(ifm)%pi0            (1,1,1)    &
      ,basic_g(ifm)%pp             (1,1,1)    &
      ,basic_g(ifm)%rv             (1,1,1)    &

      ,leaf_g(ifm)%seatp           (1,1)      &
      ,leaf_g(ifm)%seatf           (1,1)      &

      ,leaf_g(ifm)%soil_water      (1,1,1,1)  & 
      ,leaf_g(ifm)%soil_energy     (1,1,1,1)  &
      ,leaf_g(ifm)%soil_text       (1,1,1,1)  &
      ,leaf_g(ifm)%sfcwater_mass   (1,1,1,1)  &
      ,leaf_g(ifm)%sfcwater_energy (1,1,1,1)  &
      ,leaf_g(ifm)%sfcwater_depth  (1,1,1,1)  &
      ,leaf_g(ifm)%ustar           (1,1,1)    &
      ,leaf_g(ifm)%tstar           (1,1,1)    &
      ,leaf_g(ifm)%rstar           (1,1,1)    &
      ,leaf_g(ifm)%veg_fracarea    (1,1,1)    &
      ,leaf_g(ifm)%veg_lai         (1,1,1)    &
      ,leaf_g(ifm)%veg_tai         (1,1,1)    &
      ,leaf_g(ifm)%veg_rough       (1,1,1)    &
      ,leaf_g(ifm)%veg_height      (1,1,1)    &
      ,leaf_g(ifm)%veg_albedo      (1,1,1)    &
      ,leaf_g(ifm)%patch_area      (1,1,1)    &
      ,leaf_g(ifm)%patch_rough     (1,1,1)    &
      ,leaf_g(ifm)%patch_wetind    (1,1,1)    &
      ,leaf_g(ifm)%leaf_class      (1,1,1)    &
      ,leaf_g(ifm)%soil_rough      (1,1,1)    &
      ,leaf_g(ifm)%sfcwater_nlev   (1,1,1)    &
      ,leaf_g(ifm)%stom_resist     (1,1,1)    &
      ,leaf_g(ifm)%ground_rsat     (1,1,1)    &
      ,leaf_g(ifm)%ground_rvap     (1,1,1)    &
      ,leaf_g(ifm)%veg_water       (1,1,1)    &
      ,leaf_g(ifm)%veg_temp        (1,1,1)    &
      ,leaf_g(ifm)%can_rvap        (1,1,1)    &
      ,leaf_g(ifm)%can_temp        (1,1,1)    & 
      ,leaf_g(ifm)%veg_ndvip       (1,1,1)    &
      ,leaf_g(ifm)%veg_ndvic       (1,1,1)    & 
      ,leaf_g(ifm)%veg_ndvif       (1,1,1)    &
      ,leaf_g(ifm)%snow_mass       (1,1)      &
      ,leaf_g(ifm)%snow_depth      (1,1)      &

      ,scratch%scr1     (1+0*isiz)  ,scratch%scr1       (1+1*isiz)  &
      ,scratch%scr1     (1+2*isiz)  ,scratch%scr1       (1+3*isiz)  &
      ,scratch%scr1     (1+4*isiz)  ,grid_g(ifm)%glat   (1,1)       &
      ,grid_g(ifm)%glon (1,1)       ,grid_g(ifm)%topzo  (1,1)       &
      ,grid_g(ifm)%lpw (1,1)        )

! Assignment section for NOFILE leaf-2 variables

   if (icm > 0) then
    
      if( nofilflg(ifm) == 0) then
         
         ! Assign values from coarse grid cells and patches

         do ipat = 1,npatch
            do j = 1,nnyp(ifm)
               do i = 1,nnxp(ifm)
                  ic = ipm(i,ifm)
                  jc = jpm(j,ifm) 
   
                  do k = 1,nzg
                     leaf_g(ifm)%soil_water           (k,i,j,ipat) = &
                        leaf_g(icm)%soil_water      (k,ic,jc,ipat)
                     leaf_g(ifm)%soil_energy          (k,i,j,ipat) = &
                        leaf_g(icm)%soil_energy     (k,ic,jc,ipat)
                  enddo

                  do k = 1,nzs
                     leaf_g(ifm)%sfcwater_mass        (k,i,j,ipat) = &
                        leaf_g(icm)%sfcwater_mass   (k,ic,jc,ipat)
                     leaf_g(ifm)%sfcwater_energy      (k,i,j,ipat) = &
                        leaf_g(icm)%sfcwater_energy (k,ic,jc,ipat)  
                     leaf_g(ifm)%sfcwater_depth       (k,i,j,ipat) = &
                        leaf_g(icm)%sfcwater_depth  (k,ic,jc,ipat)
                  enddo

                  leaf_g(ifm)%veg_fracarea         (i,j,ipat) = &
                     leaf_g(icm)%veg_fracarea    (ic,jc,ipat)
                  leaf_g(ifm)%veg_lai              (i,j,ipat) = &
                     leaf_g(icm)%veg_lai         (ic,jc,ipat)
                  leaf_g(ifm)%veg_tai              (i,j,ipat) = &
                     leaf_g(icm)%veg_tai         (ic,jc,ipat)
                  leaf_g(ifm)%veg_rough            (i,j,ipat) = &
                     leaf_g(icm)%veg_rough       (ic,jc,ipat)
                  leaf_g(ifm)%veg_height           (i,j,ipat) = &
                     leaf_g(icm)%veg_height      (ic,jc,ipat)
                  leaf_g(ifm)%veg_albedo           (i,j,ipat) = &
                     leaf_g(icm)%veg_albedo      (ic,jc,ipat)
                  leaf_g(ifm)%patch_rough          (i,j,ipat) = &
                     leaf_g(icm)%patch_rough     (ic,jc,ipat)
                  leaf_g(ifm)%patch_wetind         (i,j,ipat) = &
                     leaf_g(icm)%patch_wetind    (ic,jc,ipat)
                  leaf_g(ifm)%soil_rough           (i,j,ipat) = &
                     leaf_g(icm)%soil_rough      (ic,jc,ipat)
                  leaf_g(ifm)%sfcwater_nlev        (i,j,ipat) = &
                     leaf_g(icm)%sfcwater_nlev   (ic,jc,ipat)
                  leaf_g(ifm)%stom_resist          (i,j,ipat) = &
                     leaf_g(icm)%stom_resist     (ic,jc,ipat) 
                  leaf_g(ifm)%ground_rsat          (i,j,ipat) = &
                     leaf_g(icm)%ground_rsat     (ic,jc,ipat)
                  leaf_g(ifm)%ground_rvap          (i,j,ipat) = &
                     leaf_g(icm)%ground_rvap     (ic,jc,ipat)
                  leaf_g(ifm)%veg_water            (i,j,ipat) = &
                     leaf_g(icm)%veg_water       (ic,jc,ipat)
                  leaf_g(ifm)%veg_temp             (i,j,ipat) = &
                     leaf_g(icm)%veg_temp        (ic,jc,ipat) 
                  leaf_g(ifm)%can_rvap             (i,j,ipat) = &
                     leaf_g(icm)%can_rvap        (ic,jc,ipat)
                  leaf_g(ifm)%can_temp             (i,j,ipat) = &
                     leaf_g(icm)%can_temp        (ic,jc,ipat) 
                  leaf_g(ifm)%veg_ndvic            (i,j,ipat) = &
                     leaf_g(icm)%veg_ndvic       (ic,jc,ipat)

               enddo
            enddo
         enddo
      
      elseif(nofilflg(ifm) == 1) then
      
         ! Interpolate from coarse grid. We can interpolate water patch directly.
         !   For land patches, do this by first averaging all
         !   coarse grid land patches, interpolate, then assign back to all
         !   fine grid land patches.
         
         call patch_interp(icm,ifm  &
            ,nzg,nnxp(icm),nnyp(icm),npatch,nzg,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%soil_water(1,1,1,1),leaf_g(ifm)%soil_water(1,1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )
         call patch_interp(icm,ifm  &
            ,nzg,nnxp(icm),nnyp(icm),npatch,nzg,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%soil_energy(1,1,1,1),leaf_g(ifm)%soil_energy(1,1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )

         call patch_interp(icm,ifm  &
            ,nzs,nnxp(icm),nnyp(icm),npatch,nzs,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%sfcwater_mass(1,1,1,1),leaf_g(ifm)%sfcwater_mass(1,1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )
         call patch_interp(icm,ifm  &
            ,nzs,nnxp(icm),nnyp(icm),npatch,nzs,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%sfcwater_energy(1,1,1,1),leaf_g(ifm)%sfcwater_energy(1,1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )
         call patch_interp(icm,ifm  &
            ,nzs,nnxp(icm),nnyp(icm),npatch,nzs,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%sfcwater_depth(1,1,1,1),leaf_g(ifm)%sfcwater_depth(1,1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )
         
         call patch_interp(icm,ifm  &
            ,1,nnxp(icm),nnyp(icm),npatch,1,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%veg_fracarea(1,1,1),leaf_g(ifm)%veg_fracarea(1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )
         call patch_interp(icm,ifm  &
            ,1,nnxp(icm),nnyp(icm),npatch,1,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%veg_lai(1,1,1),leaf_g(ifm)%veg_lai(1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )
         call patch_interp(icm,ifm  &
            ,1,nnxp(icm),nnyp(icm),npatch,1,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%veg_tai(1,1,1),leaf_g(ifm)%veg_tai(1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )
         call patch_interp(icm,ifm  &
            ,1,nnxp(icm),nnyp(icm),npatch,1,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%veg_rough(1,1,1),leaf_g(ifm)%veg_rough(1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )
         call patch_interp(icm,ifm  &
            ,1,nnxp(icm),nnyp(icm),npatch,1,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%veg_height(1,1,1),leaf_g(ifm)%veg_height(1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )
         call patch_interp(icm,ifm  &
            ,1,nnxp(icm),nnyp(icm),npatch,1,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%veg_albedo(1,1,1),leaf_g(ifm)%veg_albedo(1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )
         call patch_interp(icm,ifm  &
            ,1,nnxp(icm),nnyp(icm),npatch,1,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%patch_rough(1,1,1),leaf_g(ifm)%patch_rough(1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )
         call patch_interp(icm,ifm  &
            ,1,nnxp(icm),nnyp(icm),npatch,1,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%veg_fracarea(1,1,1),leaf_g(ifm)%veg_fracarea(1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )
         call patch_interp(icm,ifm  &
            ,1,nnxp(icm),nnyp(icm),npatch,1,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%patch_wetind(1,1,1),leaf_g(ifm)%patch_wetind(1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )
         call patch_interp(icm,ifm  &
            ,1,nnxp(icm),nnyp(icm),npatch,1,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%soil_rough(1,1,1),leaf_g(ifm)%soil_rough(1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )
         call patch_interp(icm,ifm  &
            ,1,nnxp(icm),nnyp(icm),npatch,1,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%sfcwater_nlev(1,1,1),leaf_g(ifm)%sfcwater_nlev(1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )
         call patch_interp(icm,ifm  &
            ,1,nnxp(icm),nnyp(icm),npatch,1,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%stom_resist(1,1,1),leaf_g(ifm)%stom_resist(1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )
         call patch_interp(icm,ifm  &
            ,1,nnxp(icm),nnyp(icm),npatch,1,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%ground_rsat(1,1,1),leaf_g(ifm)%ground_rsat(1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )
         call patch_interp(icm,ifm  &
            ,1,nnxp(icm),nnyp(icm),npatch,1,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%ground_rvap(1,1,1),leaf_g(ifm)%ground_rvap(1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )
         call patch_interp(icm,ifm  &
            ,1,nnxp(icm),nnyp(icm),npatch,1,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%veg_water(1,1,1),leaf_g(ifm)%veg_water(1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )
         call patch_interp(icm,ifm  &
            ,1,nnxp(icm),nnyp(icm),npatch,1,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%veg_temp(1,1,1),leaf_g(ifm)%veg_temp(1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )
         call patch_interp(icm,ifm  &
            ,1,nnxp(icm),nnyp(icm),npatch,1,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%can_rvap(1,1,1),leaf_g(ifm)%can_rvap(1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )
         call patch_interp(icm,ifm  &
            ,1,nnxp(icm),nnyp(icm),npatch,1,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%can_temp(1,1,1),leaf_g(ifm)%can_temp(1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )
          call patch_interp(icm,ifm  &
            ,1,nnxp(icm),nnyp(icm),npatch,1,nnxp(ifm),nnyp(ifm),npatch  &
            ,leaf_g(icm)%veg_ndvic(1,1,1),leaf_g(ifm)%veg_ndvic(1,1,1) &
            ,leaf_g(icm)%patch_area(1,1,1),leaf_g(icm)%patch_area(1,1,1) &
            ,scratch%vt3da(1),scratch%vt3db(1),scratch%vt2da(1),scratch%vt2db(1) )
      
      endif

   endif

! Override any of the above variable assignments by user-specified changes
! to subroutine sfcinit_nofile_user.

   call sfcinit_nofile_user(nnzp(ifm),nnxp(ifm),nnyp(ifm)       &
      ,nzg,nzs,npatch,ifm              &
      ,basic_g(ifm)%theta (1,1,1) ,basic_g(ifm)%pi0 (1,1,1)  &
      ,basic_g(ifm)%pp    (1,1,1) ,basic_g(ifm)%rv  (1,1,1)  &

      ,leaf_g(ifm)%soil_water      (1,1,1,1)  & 
      ,leaf_g(ifm)%soil_energy     (1,1,1,1)  &
      ,leaf_g(ifm)%soil_text       (1,1,1,1)  &
      ,leaf_g(ifm)%sfcwater_mass   (1,1,1,1)  &
      ,leaf_g(ifm)%sfcwater_energy (1,1,1,1)  &
      ,leaf_g(ifm)%sfcwater_depth  (1,1,1,1)  &
      ,leaf_g(ifm)%ustar           (1,1,1)    &
      ,leaf_g(ifm)%tstar           (1,1,1)    &
      ,leaf_g(ifm)%rstar           (1,1,1)    &
      ,leaf_g(ifm)%veg_fracarea    (1,1,1)    &
      ,leaf_g(ifm)%veg_lai         (1,1,1)    &
      ,leaf_g(ifm)%veg_tai         (1,1,1)    &
      ,leaf_g(ifm)%veg_rough       (1,1,1)    &
      ,leaf_g(ifm)%veg_height      (1,1,1)    &
      ,leaf_g(ifm)%veg_albedo      (1,1,1)    &
      ,leaf_g(ifm)%patch_area      (1,1,1)    &
      ,leaf_g(ifm)%patch_rough     (1,1,1)    &
      ,leaf_g(ifm)%patch_wetind    (1,1,1)    &
      ,leaf_g(ifm)%leaf_class      (1,1,1)    &
      ,leaf_g(ifm)%soil_rough      (1,1,1)    &
      ,leaf_g(ifm)%sfcwater_nlev   (1,1,1)    &
      ,leaf_g(ifm)%stom_resist     (1,1,1)    &
      ,leaf_g(ifm)%ground_rsat     (1,1,1)    &
      ,leaf_g(ifm)%ground_rvap     (1,1,1)    &
      ,leaf_g(ifm)%veg_water       (1,1,1)    &
      ,leaf_g(ifm)%veg_temp        (1,1,1)    &
      ,leaf_g(ifm)%can_rvap        (1,1,1)    &
      ,leaf_g(ifm)%can_temp        (1,1,1)    & 
      ,leaf_g(ifm)%veg_ndvip       (1,1,1)    &
      ,leaf_g(ifm)%veg_ndvic       (1,1,1)    & 
      ,leaf_g(ifm)%veg_ndvif       (1,1,1)    &
      ,leaf_g(ifm)%snow_mass       (1,1)      &
      ,leaf_g(ifm)%snow_depth      (1,1)      &

      ,scratch%scr1     (1+0*isiz)  ,scratch%scr1       (1+1*isiz)  &
      ,scratch%scr1     (1+2*isiz)  ,scratch%scr1       (1+3*isiz)  &
      ,scratch%scr1     (1+4*isiz)  ,grid_g(ifm)%glat   (1,1)       &
      ,grid_g(ifm)%glon (1,1)       ,grid_g(ifm)%topzo  (1,1)       &
      ,grid_g(ifm)%lpw (1,1)        )

enddo

return
end




!******************************************************************************

subroutine patch_interp(icm,ifm,nc1,nc2,nc3,nc4,nf1,nf2,nf3,nf4 &
                       ,ac,af,pareac,pareaf,avgc,avgf,slabc,slabf)

use mem_scratch

implicit none

integer :: icm,ifm,nc1,nc2,nc3,nc4,nf1,nf2,nf3,nf4
real :: ac(nc1,nc2,nc3,nc4), af(nf1,nf2,nf3,nf4)
real :: pareac(nc2,nc3,nc4), pareaf(nf2,nf3,nf4)
real :: avgc(nc1,nc2,nc3),   avgf(nf1,nf2,nf3)
real :: slabc(nc2,nc3), slabf(nf2,nf3)

integer :: k,i,j

! Average coarse grid field over all land patches

call patch_land_average(nc1,nc2,nc3,nc4  &
                       ,pareac(1,1,1),ac(1,1,1,1),avgc(1,1,1))

! Interpolate patch-averaged to fine grid

do k=1,nc1    ! nc1 and nf1 are the same

   do j=1,nc3
      do i=1,nc2
         slabc(i,j)=avgc(k,i,j)
      enddo
   enddo
   
   call fmint2d(icm,ifm,'t',slabc(1,1),slabf(1,1))

   do j=1,nf3
      do i=1,nf2
         avgf(k,i,j)=slabf(i,j)
      enddo
   enddo
   
enddo 

! Fill fine grid field back into all land patches

call patch_land_unaverage(nf1,nf2,nf3,nf4,avgf(1,1,1),af(1,1,1,1))

return
end

!******************************************************************************

subroutine patch_minsize(n2,n3,npat,patch_area)

implicit none
integer :: n2,n3,npat,i,j,ipat,jpat

real :: orig_size
real, dimension(n2,n3,npat) :: patch_area

do j = 1,n3
   do i = 1,n2
      do ipat = 2,npat
         if (patch_area(i,j,ipat) .gt. 0. .and.  &
             patch_area(i,j,ipat) .lt. .01) then

            orig_size = patch_area(i,j,ipat)
            patch_area(i,j,ipat) = 0.

            do jpat = 1,npat
               if (jpat .ne. ipat) then
                  patch_area(i,j,jpat) = patch_area(i,j,jpat)  &
                     / (1. - orig_size)
               endif
            enddo
         endif
      enddo
   enddo
enddo

return
end

