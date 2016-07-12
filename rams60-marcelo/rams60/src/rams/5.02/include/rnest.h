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
integer                            :: nrzflg
integer, dimension(nxpmax,maxgrds) :: ipm
integer, dimension(nypmax,maxgrds) :: jpm 
integer, dimension(nzpmax,maxgrds) :: nrz,kpm 
real, dimension(nxpmax,maxgrds)    :: ei1,ei2,ei3,ei4,ei5,ei6,ei7 
real, dimension(nypmax,maxgrds)    :: ej1,ej2,ej3,ej4,ej5,ej6,ej7
real, dimension(nzpmax,maxgrds)    :: ek1,ek2,ek3,ek4,ek5,ek6,ek7 
real, dimension(nzpmax,maxgrds,4)  :: fbcf
common/nest/nrzflg,nrz,ipm,jpm,kpm  &
           ,ei1,ei2,ei3,ei4,ei5,ei6,ei7,ej1,ej2,ej3,ej4,ej5,ej6,ej7  &
           ,ek1,ek2,ek3,ek4,ek5,ek6,ek7,fbcf
!---------------------------------------------------------------------------
