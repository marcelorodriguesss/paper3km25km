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
!     Common block include file for the   R A M S   model - bin microphysics
!---------------------------------------------------------------------------
!
! For stratocumulus simulations use NB,LPX=25,
! For convective simulations use NB,LPX=34 (inc. L&L binary breakup)
! NS is the number of the first vector that contains "bins information"
! The first vector is for saturation excess
! The second vector is for saturation excess with respect to ice in LEVEL>4
! The next vector is for CCN concentration
! The next vector is for IN already activated LEVEL>4
! The next vector is for IN already activated thru contact LEVEL>4
!
!  LN2 is the number of bins for CCN when using Yan's nucleation
!
!parameter :: NB=25,LPX=25,LPA=25,LCO=19,LPA1=LPA+1,NSH=6
integer, parameter :: MB=36,LB=MB,LRK=15,LCO=19,LN2=67
!parameter :: MB=36,LB=36,LRK=15,LCO=19,LN2=67
integer, parameter :: NSDSW=1,NSDSI=2,NSCCN=3,NSIN=4,NSIC=5,NSH=6
integer, parameter :: LK2=6,LKI=34

