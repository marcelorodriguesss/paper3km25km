!f90
!############################# Change Log ##################################
! 4.3.0.0
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000 - All Rights Reserved
!  Regional Atmospheric Modeling System - RAMS
!  Mission Research Corporation / *ASTeR Division
!###########################################################################

!---------------------------------------------------------------------------
!    COMMON block for parallel stuff
!---------------------------------------------------------------------------
integer                                         :: mainnum,nmachs,iparallel  &
                                                  ,load_bal
integer, dimension(maxmach)                     :: machnum,nbuff_nest1  &
                                                  ,newbuff_nest1
integer, dimension(maxmach,maxgrds)             :: nxbeg,nxend,nybeg  &
                                                  ,nyend,nxbegc,nxendc  &
                                                  ,nybegc,nyendc,ixoff  &
                                                  ,iyoff,npxy,ibcflg  &
                                                  ,nestflg,ifeednode  &
                                                  ,ixb,ixe,iyb,iye
integer, dimension(maxmach,maxgrds,4)           :: nextnode
integer, dimension(5,7,maxgrds,maxmach,maxmach) :: inode_paths_master
integer, dimension(6,maxgrds,maxmach,maxmach)   :: iget_paths_master
integer, dimension(2,maxmach,maxmach)           :: lbc_buffs
real, dimension(maxmach)                        :: hperf
real, dimension(maxmach,maxgrds)                :: perf
common/parallel/nxbeg,nxend,nybeg,nyend,nxbegc,nxendc,nybegc,nyendc  &
               ,ixoff,iyoff,npxy,mainnum,machnum,perf,nmachs,hperf  &
               ,ibcflg,nestflg,iparallel,ifeednode,nextnode  &
               ,ixb,ixe,iyb,iye  &
               ,inode_paths_master,iget_paths_master  &
               ,lbc_buffs,nbuff_nest1,newbuff_nest1,load_bal
!---------------------------------------------------------------------------
character(len=80), dimension(maxmach) :: hosts
common/parac/hosts
!---------------------------------------------------------------------------
character(len=3) :: mplib
common/parac2/mplib
!---------------------------------------------------------------------------
