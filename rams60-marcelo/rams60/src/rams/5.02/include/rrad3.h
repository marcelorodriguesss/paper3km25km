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
integer                         :: nrad,narad,nsolb,nb,ng,jday
integer, parameter              :: mb=8,mg=3,mk=7,ncog=5,ncb=2,npartob=13  &
                                  ,npartg=7,namax=10
integer, dimension(mg,mb)       :: npsb
real                            :: solfac
real, dimension(mb)             :: nuum,ralcs,solar1,solar0,a0,a1,a2,a3  &
                                  ,wlenlo,wlenhi
real, dimension(150)            :: exptabc
real, dimension(mg,mb)          :: prf,trf,ulim
real, dimension(mg,mk,mb)       :: wght,xp,alpha,beta
real, dimension(ncog,mb,npartob) :: ocoef
real, dimension(ncb,mb,npartob) :: bcoef
real, dimension(ncog,mb,npartg) :: gcoef
common/rrad3/nrad,narad,nsolb,nb,ng,jday,npsb  &
            ,solfac,ralcs,solar1,solar0,nuum,a0,a1,a2,a3,wlenlo,wlenhi  &
            ,prf,trf,ulim,wght,xp,alpha,beta,ocoef,bcoef,gcoef,exptabc
!---------------------------------------------------------------------------
