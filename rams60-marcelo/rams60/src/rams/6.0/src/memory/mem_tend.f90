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


Module mem_tend


   Type tend_vars

   real, pointer, dimension(:) :: &
         ut, vt, wt, pt, tht, rtt  &
        ,rct, rrt, rpt, rst, rat, rgt ,rht  &
        ,cct, crt, cpt, cst, cat, cgt ,cht  &
        ,cccnt, cifnt, tket, epst
   
   End Type
   
   type (tend_vars) :: tend

Contains
!---------------------------------------------------------------

   subroutine alloc_tend(nmzp,nmxp,nmyp,ngrs,naddsc,proc_type)

   use mem_basic
   use mem_scalar
   use mem_micro
   use mem_turb

   implicit none
   
   integer, dimension (*) :: nmzp,nmxp,nmyp
   integer :: ngrs,proc_type,naddsc

   integer :: ng,ntpts,nsc

print*, 'enter alloc_tend'

!         Find the maximum number of grid points needed for any grid.

   if(proc_type==1) then
      ntpts=1
   else
      ntpts=0
      do ng=1,ngrs
         ntpts=max( nmxp(ng)*nmyp(ng)*nmzp(ng),ntpts )
      enddo
   endif

! Allocate arrays based on options (if necessary).
!   Do not need these arrays if it is a master process in a parallel run,
!      so just allocate to 1 word.

!!!!!  WE ARE ONLY CHECKING GRID 1 !!!!!!!!!
!!!!!    All grids must have same scalars defined !!!!!!!

   if (associated(basic_g(1)%up))      allocate (tend%ut(ntpts))
   if (associated(basic_g(1)%vp))      allocate (tend%vt(ntpts))
   if (associated(basic_g(1)%wp))      allocate (tend%wt(ntpts))
   if (associated(basic_g(1)%pp))      allocate (tend%pt(ntpts))
      
   if (associated(basic_g(1)%thp))     allocate (tend%tht(ntpts))
   if (associated(basic_g(1)%rtp))     allocate (tend%rtt(ntpts))
   if (associated(micro_g(1)%rcp))     allocate (tend%rct(ntpts))
   if (associated(micro_g(1)%rrp))     allocate (tend%rrt(ntpts))
   if (associated(micro_g(1)%rpp))     allocate (tend%rpt(ntpts))
   if (associated(micro_g(1)%rsp))     allocate (tend%rst(ntpts))
   if (associated(micro_g(1)%rap))     allocate (tend%rat(ntpts))
   if (associated(micro_g(1)%rgp))     allocate (tend%rgt(ntpts))
   if (associated(micro_g(1)%rhp))     allocate (tend%rht(ntpts))
   if (associated(micro_g(1)%ccp))     allocate (tend%cct(ntpts))
   if (associated(micro_g(1)%crp))     allocate (tend%crt(ntpts))
   if (associated(micro_g(1)%cpp))     allocate (tend%cpt(ntpts))
   if (associated(micro_g(1)%csp))     allocate (tend%cst(ntpts))
   if (associated(micro_g(1)%cap))     allocate (tend%cat(ntpts))
   if (associated(micro_g(1)%cgp))     allocate (tend%cgt(ntpts))
   if (associated(micro_g(1)%chp))     allocate (tend%cht(ntpts))
   if (associated(micro_g(1)%cccnp))   allocate (tend%cccnt(ntpts))
   if (associated(micro_g(1)%cifnp))   allocate (tend%cifnt(ntpts))
   if (associated(turb_g(1)%tkep))     allocate (tend%tket(ntpts))
   if (associated(turb_g(1)%epsp))     allocate (tend%epst(ntpts))
      
   do nsc=1,naddsc
      if (associated(scalar_g(nsc,1)%sclp).and.  &
             (.not.associated(scalar_g(nsc,1)%sclt)))  &
              allocate (scalar_g(nsc,1)%sclt(ntpts))
      do ng=2,ngrs
         scalar_g(nsc,ng)%sclt => scalar_g(nsc,1)%sclt
      enddo
   enddo


   return
   end subroutine

!---------------------------------------------------------------
               
   subroutine nullify_tend(naddsc)
   
   use mem_scalar

   implicit none

   integer :: naddsc   
   
   integer :: nsc

! Deallocate all tendency arrays

   if (associated(tend%ut))   nullify (tend%ut)
   if (associated(tend%vt))   nullify (tend%vt)
   if (associated(tend%wt))   nullify (tend%wt)
   if (associated(tend%pt))   nullify (tend%pt)
   if (associated(tend%tht))  nullify (tend%tht)
   if (associated(tend%tht))  nullify (tend%rtt)
   if (associated(tend%rtt))  nullify (tend%rtt)
   if (associated(tend%rct))  nullify (tend%rct)
   if (associated(tend%rrt))  nullify (tend%rrt)
   if (associated(tend%rpt))  nullify (tend%rpt)
   if (associated(tend%rst))  nullify (tend%rst)
   if (associated(tend%rat))  nullify (tend%rat)
   if (associated(tend%rgt))  nullify (tend%rgt)
   if (associated(tend%rht))  nullify (tend%rht)
   if (associated(tend%cct))  nullify (tend%cct)
   if (associated(tend%crt))  nullify (tend%crt)
   if (associated(tend%cpt))  nullify (tend%cpt)
   if (associated(tend%cst))  nullify (tend%cst)
   if (associated(tend%cat))  nullify (tend%cat)
   if (associated(tend%cgt))  nullify (tend%cgt)
   if (associated(tend%cht))  nullify (tend%cht)
   if (associated(tend%cccnt))nullify (tend%cccnt)
   if (associated(tend%cifnt))nullify (tend%cifnt)

   if (associated(tend%tket)) nullify (tend%tket)
   if (associated(tend%epst)) nullify (tend%epst)

   do nsc=1,naddsc
      if (associated(scalar_g(nsc,1)%sclt)) nullify (scalar_g(nsc,1)%sclt)
   enddo
        
   return
   end subroutine
!---------------------------------------------------------------
               
   subroutine dealloc_tend(naddsc)
   
   use mem_scalar

   implicit none

   integer :: naddsc   
   
   integer :: nsc

! Deallocate all tendency arrays

   if (associated(tend%ut))   deallocate (tend%ut)
   if (associated(tend%vt))   deallocate (tend%vt)
   if (associated(tend%wt))   deallocate (tend%wt)
   if (associated(tend%pt))   deallocate (tend%pt)
   if (associated(tend%tht))  deallocate (tend%tht)
   if (associated(tend%tht))  deallocate (tend%rtt)
   if (associated(tend%rtt))  deallocate (tend%rtt)
   if (associated(tend%rct))  deallocate (tend%rct)
   if (associated(tend%rrt))  deallocate (tend%rrt)
   if (associated(tend%rpt))  deallocate (tend%rpt)
   if (associated(tend%rst))  deallocate (tend%rst)
   if (associated(tend%rat))  deallocate (tend%rat)
   if (associated(tend%rgt))  deallocate (tend%rgt)
   if (associated(tend%rht))  deallocate (tend%rht)
   if (associated(tend%cct))  deallocate (tend%cct)
   if (associated(tend%crt))  deallocate (tend%crt)
   if (associated(tend%cpt))  deallocate (tend%cpt)
   if (associated(tend%cst))  deallocate (tend%cst)
   if (associated(tend%cat))  deallocate (tend%cat)
   if (associated(tend%cgt))  deallocate (tend%cgt)
   if (associated(tend%cht))  deallocate (tend%cht)
   if (associated(tend%cccnt))deallocate (tend%cccnt)
   if (associated(tend%cifnt))deallocate (tend%cifnt)

   if (associated(tend%tket)) deallocate (tend%tket)
   if (associated(tend%epst)) deallocate (tend%epst)

   do nsc=1,naddsc
      if (associated(scalar_g(nsc,1)%sclt)) deallocate (scalar_g(nsc,1)%sclt)
   enddo
        
   return
   end subroutine

!---------------------------------------------------------------
               
   subroutine filltab_tend(basic,micro,turb,scalar,naddsc,ng)

   use mem_basic
   use mem_micro
   use mem_turb
   use mem_scalar
   use var_tables

   implicit none

   type (basic_vars) :: basic
   type (micro_vars) :: micro
   type (turb_vars) :: turb
   type (scalar_vars) :: scalar(*)
   integer :: naddsc,ng
   
   integer :: nsc
   character (len=7) :: sname

! Fill pointers to scalar arrays into scalar tables

   if (associated(tend%tht))  &
      call vtables_scalar (basic%thp(1,1,1),tend%tht(1),ng,'THP')
   if (associated(tend%rtt))  &
      call vtables_scalar (basic%rtp(1,1,1),tend%rtt(1),ng,'RTP')
   if (associated(tend%rct))  &
      call vtables_scalar (micro%rcp(1,1,1),tend%rct(1),ng,'RCP')
   if (associated(tend%rrt))  &
      call vtables_scalar (micro%rrp(1,1,1),tend%rrt(1),ng,'RRP')
   if (associated(tend%rpt))  &
      call vtables_scalar (micro%rpp(1,1,1),tend%rpt(1),ng,'RPP')
   if (associated(tend%rst))  &
      call vtables_scalar (micro%rsp(1,1,1),tend%rst(1),ng,'RSP')
   if (associated(tend%rat))  &
      call vtables_scalar (micro%rap(1,1,1),tend%rat(1),ng,'RAP')
   if (associated(tend%rgt))  &
      call vtables_scalar (micro%rgp(1,1,1),tend%rgt(1),ng,'RGP')
   if (associated(tend%rht))  &
      call vtables_scalar (micro%rhp(1,1,1),tend%rht(1),ng,'RHP')
   if (associated(tend%cct))  &
      call vtables_scalar (micro%ccp(1,1,1),tend%cct(1),ng,'CCP')
   if (associated(tend%crt))  &
      call vtables_scalar (micro%crp(1,1,1),tend%crt(1),ng,'CRP')
   if (associated(tend%cpt))  &
      call vtables_scalar (micro%cpp(1,1,1),tend%cpt(1),ng,'CPP')
   if (associated(tend%cst))  &
      call vtables_scalar (micro%csp(1,1,1),tend%cst(1),ng,'CSP')
   if (associated(tend%cat))  &
      call vtables_scalar (micro%cap(1,1,1),tend%cat(1),ng,'CAP')
   if (associated(tend%cgt))  &
      call vtables_scalar (micro%cgp(1,1,1),tend%cgt(1),ng,'CGP')
   if (associated(tend%cht))  &
      call vtables_scalar (micro%chp(1,1,1),tend%cht(1),ng,'CHP')
   if (associated(tend%cccnt))  &
      call vtables_scalar (micro%cccnp(1,1,1),tend%cccnt(1),ng,'CCCNP')
   if (associated(tend%cifnt))  &
      call vtables_scalar (micro%cifnp(1,1,1),tend%cifnt(1),ng,'CIFNP')

   if( associated(tend%tket))  &
      call vtables_scalar (turb%tkep(1,1,1),tend%tket(1),ng,'TKEP')
   if( associated(tend%epst))  &
      call vtables_scalar (turb%epsp(1,1,1),tend%epst(1),ng,'EPSP')
        
   do nsc=1,naddsc
      write(sname,'(a4,i3.3)') 'SCLP',nsc
      if (associated(scalar(nsc)%sclt))  &
         call vtables_scalar (scalar(nsc)%sclp(1,1,1)  &
                             ,scalar(nsc)%sclt(1),ng,sname)
   enddo
        
   return
   end subroutine

End Module mem_tend
