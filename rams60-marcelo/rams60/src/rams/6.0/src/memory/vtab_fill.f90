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


   subroutine vtables2(var,varm,ng,npts,imean,tabstr)

   use var_tables
      
   implicit none
   real, target :: var,varm
   integer, intent(in) :: ng,npts,imean
   character (len=*), intent(in) :: tabstr

   character (len=80) ::line
   character (len=1) ::toksep=':', cdimen,ctype
   character (len=32) ::tokens(10)
   character (len=8) :: cname,ctab
   
   integer :: ntok,nt,nv
     
   call tokenize1(tabstr,tokens,ntok,toksep)
   
   num_var(ng)=num_var(ng)+1
   nv=num_var(ng)

   vtab_r(nv,ng)%var_p => var
   vtab_r(nv,ng)%var_m => varm

  
   vtab_r(nv,ng)%name=tokens(1)
   vtab_r(nv,ng)%npts=npts
   read(tokens(2),*) vtab_r(nv,ng)%idim_type
   !print*,'tab:',nv,ng,vtab_r(nv,ng)%name ,vtab_r(nv,ng)%npts
     
   vtab_r(nv,ng)%ihist=0
   vtab_r(nv,ng)%ianal=0
   vtab_r(nv,ng)%imean=imean
   vtab_r(nv,ng)%ilite=0
   vtab_r(nv,ng)%impti=0
   vtab_r(nv,ng)%impt1=0
   vtab_r(nv,ng)%impt2=0
   vtab_r(nv,ng)%impt3=0
   vtab_r(nv,ng)%irecycle=0

   do nt=3,ntok
      ctab=tokens(nt)         
      
      if(ctab == 'hist' ) then
      	 vtab_r(nv,ng)%ihist=1
      elseif(ctab == 'anal' ) then
      	 vtab_r(nv,ng)%ianal=1
      elseif(ctab == 'lite' ) then
      	 vtab_r(nv,ng)%ilite=1
      elseif(ctab == 'mpti' ) then
      	 vtab_r(nv,ng)%impti=1
      elseif(ctab == 'mpt1' ) then
      	 vtab_r(nv,ng)%impt1=1
      elseif(ctab == 'mpt2' ) then
      	 vtab_r(nv,ng)%impt2=1
      elseif(ctab == 'mpt3' ) then
      	 vtab_r(nv,ng)%impt3=1
      elseif(ctab == 'recycle' ) then
      	 vtab_r(nv,ng)%irecycle=1
      else
         print*, 'Illegal table specification for var:', tokens(1),ctab
         stop 'bad var table'
      endif

   enddo
  
   return
   end

!-------------------------------------------------------------------------

subroutine lite_varset()

use var_tables
use io_params

implicit none

integer :: nv,ng,nvl,ifound


! Loop over each variable input in namelist "LITE_VARS" and set
!   lite flag in var_tables

do ng = 1,nvgrids   
   vtab_r(1:num_var(ng),ng)%ilite = 0
enddo

do nvl=1,nlite_vars
   ifound=0
      
   do ng=1,nvgrids
   
      do nv=1,num_var(ng)
   
         if (vtab_r(nv,ng)%name == lite_vars(nvl) ) then
            vtab_r(nv,ng)%ilite = 1
            ifound=1
         endif
         
      enddo
      
   enddo
   
   if(ifound == 0) then
      print*,'!---------------------------------------------------------'
      print*,'! LITE_VARS variable does not exist in main variable table'
      print*,'!    variable name-->',lite_vars(nvl),'<--'
      print*,'!---------------------------------------------------------'
   else
      print*,'!---------------------------------------------------------'
      print*,'! LITE_VARS variable added--->',trim(lite_vars(nvl))
      print*,'!---------------------------------------------------------'
   endif
   
10 continue   
enddo

return
end
            
!-------------------------------------------------------------------------
   
   subroutine vtables_scalar(varp,vart,ng,tabstr)

   use var_tables
      
   implicit none
   real, target :: varp,vart
   integer, intent(in) :: ng
   character (len=*), intent(in) :: tabstr

   character (len=80) ::line
   character (len=1) ::toksep=':'
   character (len=32) ::tokens(10)
   character (len=16) :: cname,ctab
   
   integer :: ntok,nv,isnum,ns
     
   call tokenize1(tabstr,tokens,ntok,toksep)
   cname=tokens(1)
!   ctab=tokens(2) 
   
!    See if this scalar name is already in the table...
   
   isnum=0

!   do ns=1,num_scalar(ng)
!      if(cname == scalar_tab(ns,ng)%name) then
!      	 isnum=ns
!      	 exit
!      endif
!   enddo

!    Fill in existing table slot or make new scalar slot

!   if (isnum == 0) then
      num_scalar(ng)=num_scalar(ng)+1
      nv=num_scalar(ng)
      scalar_tab(nv,ng)%name = cname
!   else
!      nv=isnum
!   endif

   scalar_tab(nv,ng)%var_p => varp
   scalar_tab(nv,ng)%var_t => vart
  
!   if(ctab == 'sclp' ) then
!      scalar_tab(nv,ng)%varp => varp
!   elseif(ctab == 'sclt' ) then
!      scalar_tab(nv,ng)%vart => vart
!   else
!      print*, 'Illegal scalar table specification for var:', cname
!      stop 'bad scalar table'
!   endif
  
   return
   end
