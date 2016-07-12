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

subroutine plotspc (nfl,action)

use an_header

implicit none

integer :: nfl
character(len=*) :: action

include 'vcomm2.h'
include 'frame.h'
include 'v5df.h'
include 'interface.h'

real :: flat,flon,elev,xfx,yfy,zfz,ppx1,ppx2,ppy1,ppy2,glats,glatn,glonw  &
       ,glone,xref,yref,startutc
real, allocatable :: fobs(:),pred(:),fpos(:)
integer :: igunit,lastslash,ngds,ngdbv,ngdev,ngdb,ngde,ngd  &
          ,ivtime,ibegs,iends,nngd,ivvar,i1,i  &
          ,ntokfr,ntok,itok,iver,iidsta,itype,ngridg  &
          ,nplotcur,izztran,irefg,jrefg,ndum,iver_sfc,imarker,n,nclevels  &
          ,niboff,nieoff,njboff,njeoff,nnboff,nneoff,il,iv,nval
character(len=1)   :: toksepfr,toksep,cgrid,ccont
character(len=3)   :: ctime
character(len=32)  :: tokens(20)
character(len=64)  :: frtokens(50)
character(len=16)  :: gradsvar(maxfore) 
character(len=256) :: flnm,flnm1,line
character(len=15)  :: dategrads,dategrads1
character(len=4)   :: cincgrads
character(len=24)  :: cdname1(maxfore),cdunits1(maxfore)
character(len=20)  :: color

logical :: fexist(maxgrds)

data toksepfr/'/'/ toksep/':'/
data igunit /50/  ! GrADS write unit number
data dategrads1 / '00:00z00mmm1900' /

ipanl=1
numfore=0
if(action(1:5)=='STATS'.or.action(1:4)=='GRAB') iztran(1)=1

do nplot=1,maxfore

   ! allow plotting of cframe_b when nothing in cframe_a
   if(len_trim(cframe_a(nplot))==0.and.len_trim(cframe_b(nplot)).ne.0)  &
      cframe_a(nplot)='/none/'
            
   ! skip this frame if nothing to plot
   if(cframe_a(nplot)(1:1).ne.'/') goto 3
   numfore=numfore+1

   if(action(1:5)=='STATS'.or.action(1:4)=='GRAB' .or.  &
      action(1:3)=='V5D'  .or.action(1:5)=='GRADS'.or.  &
      action(1:4)=='DUMP' .or.action(1:4)=='GRIB' .or.  &
      action(1:5)=='MEDOC') then

      ! for things that can't do varied backgrounds
      iplevel(nplot)=iplevel(1)
      iztran(nplot)=iztran(1)
      tvar(nplot)=tvar(1)
      zvar(nplot)=zvar(1)
      xvar(nplot)=xvar(1)
      yvar(nplot)=yvar(1)

   elseif(action(1:5)=='SPACE') then

      if(nplot.gt.1) then
      
         ! check have vertical transformation and color information
         if(iztran(nplot)==0) iztran(nplot)=iztran(1)
         if(iplevel(nplot)==0) iplevel(nplot)=iplevel(1)
         if(len_trim(landmark(nplot))==0) landmark(nplot)=landmark(1)
         if(len_trim(colors(nplot))==0) colors(nplot)=colors(1)

         ! for required varied backgrounds vars that don't exist - set to 1
         if(len_trim(tvar(nplot))==0) tvar(nplot)=tvar(1)
         if(len_trim(zvar(nplot))==0) zvar(nplot)=zvar(1)
         if(len_trim(xvar(nplot))==0) xvar(nplot)=xvar(1)
         if(len_trim(yvar(nplot))==0) yvar(nplot)=yvar(1)
      
      else
         
         ! set global plotting values
         iwk=1
         mfill=mapfill
         if(abs(mapfill).gt.3) then
            print*,'MAPFILL must be between -3 and 3',MAPFILL
            stop 'plotspc'
         endif
         ibgnd=ibackgnd
         if(abs(ibackgnd).gt.3) then
            print*,'IBACKGND must be between -3 and 3',IBACKGND
            stop 'plotspc'
         endif
         ipinfo=ipltinfo
         if(ipanel.lt.0.or.ipanel.gt.4) then
            print*,'IPANEL must be between 0 and 4',IPANEL
            stop 'plotspc'
         endif
         ipanl=ipanel
         
         if(iztran(nplot)==0) then
            print*,'IZTRAN not specified'
            stop 'plotspc'
         endif
         if(iplevel(nplot)==0.and.iztran(nplot)==3) then
            print*,'IPLEVEL not specified'
            stop 'plotspc'
         endif
     
         ! set defaults required to map colors
         call continit ()
         
         ! get CFRAME_A attributes required to map colors
         call contset (1,'CFRAME_A',cframe_a(1))
         
         ! get CFRAME_C attributes required to map colors
         call contset (2,'CFRAME_C',cframe_a(1))
     
         ! do initial fill of colors and plot them if ipanl=0
         call gks_colors (iwk,ibgnd)
         call gks_fillcolors (1,96,114,133)
         call gks_fillcolors (2,176,194,213)
         if(ipanl==0) then
            print*
            print*,'Ploting color maps'
            call draw_colours (1,58,'General Colors')
            call frame
            call draw_colours (59,89,'Std Tile Colors')
            call frame
            call draw_colours (90,95,'Std Vector Colors')
            call frame
            call draw_colours (96,135,'Progressive Fill Colors A')
            call frame
            call draw_colours (136,175,'Progressive Line Colors A')
            call frame
            call draw_colours (176,215,'Progressive Fill Colors C')
            call frame
            call draw_colours (216,255,'Progressive Line Colors C')
            call frame
            return
         endif
         
      endif

   endif

   3 continue
enddo

call RAMS_get_idata(1,itbeg,1,ngds,nval)

if(action(1:3)=='V5D' .or.action(1:5)=='GRADS'.or.  &
   action(1:4)=='GRIB'.or.action(1:4)=='DUMP' .or.  &
   action(1:5)=='MEDOC') then
   
   if(action(1:4)/='DUMP'.and.action(1:5)/='MEDOC'.and.  &
      IZTRAN(1)==1) print*,'Warning: '  &
      ,'writing RAMS terrain following coords to output file'
   
   if(igrid(1).gt.0) then
      ngdbv=igrid(1)
      ngdev=igrid(1)
   elseif(igrid(1)==0) then
      ngdbv=1
      ngdev=ngds
   elseif(igrid(1).lt.0) then
      ngdbv=abs(igrid(1))
      ngdev=ngds
   endif
   
else

   ngdbv=1
   ngdev=1
   if(igrid(1).gt.0) then
      ngdb=igrid(1)
      ngde=igrid(1)
   elseif(igrid(1)==0) then
      ngdb=1
      ngde=ngds
   elseif(igrid(1).lt.0) then
      ngdb=abs(igrid(1))
      ngde=ngds
   endif
   
endif

do ngd=ngdbv,ngdev
   if(action(1:3)=='V5D' .or.action(1:5)=='GRADS'.or.  &
      action(1:4)=='GRIB'.or.action(1:4)=='DUMP' .or.  &
      action(1:5)=='MEDOC') then
   
      if(action(1:3)=='V5D') then
         call RAMS_v5dh (nfl,ngd)
      elseif(action(1:5)=='GRADS') then
      
         ! Would like to open it here, but need to know the number
         ! of grid points to plot to determin record length, so
         ! need to do it in iplt.f instead.
         !call RAMS_gradsbin(igunit,'OPEN')

      elseif(action(1:4)=='GRIB') then
         call RAMS_get_fdata (2,1,nngd,startutc,nval)
         call RAMS_gribfile (igunit,'OPEN',iztran(1),ngd,iyear1  &
                            ,imonth1,idate1,itime1,revpref)
      endif
      
      ngdb=ngd
      ngde=ngd
      
   endif
   
   ivtime=0
   ibegs=max(itbeg,1)
   iends=min(itend,nfl)
   iends=max(1,iends)
   do nfile=ibegs,iends,itstep
      print*
      write(*,'(a,2i3)') 'Doing file,grid - ',nfile,ngd
      
      ! If grid does not exist at this time, skip completely
            
      if((action(1:5)=='GRADS'.or.action(1:4)=='V5D') .and. &
            .not.gridinv(nfile,ngd)) go to 12
      
      ivtime=ivtime+1

      ! obtain observational data for each time requested.
      ! need to call memory allocation routines at first time
      ! increment. amount of memory needed will depend on number
      ! of times and number of variables.

      ! miss grid if no pts on that grid
      do nngd=ngdb,ngde
         igrabgrd(nngd)=0
      enddo

      if(action(1:5)=='STATS'.or.action(1:4)=='GRAB') then

         nfndx=ivtime
         if(nfile==1.or.nfile==itbeg) then
            ivvar=0
            do i1=1,maxfore
               if(cframe_a(i1)(1:1).ne.'/') goto 5
               do i=1,50
                  frtokens(i)=' '
               enddo
               call tokenize1 (cframe_a(i1),frtokens,ntokfr,toksepfr)
               if(frtokens(1)=='none') goto 5
               ivvar=ivvar+1
               stchar(ivvar)=frtokens(1)
               5 continue
            enddo

            ! maximum pts is all the surface obs plus the levels (means that
            ! some of this space will never be used
            ! fobs and pred only carry met vars as determined by the namelist
            ! fpos carries position - lat, lon, x (m), y (m), z (m agl), grid
            !NUMPTS=MAXLOC*(nplevs+1)
            numpts=0

            open(unit=31,file=GRABIN,status='old')
            18 continue
            read(31,'(a)',end=99,err=99) line
            if(line(1:1)=='#'.or.line(1:1)=='!') goto 18
            call parse(line,tokens,ntok)
            if(tokens(1)=='#'.or.tokens(1)=='!') goto 18
            if(ntok.ne.1) then
               iver=1
               rewind(31)
            else
               read(tokens(1),*) iver
            endif
            19 continue
            read(31,'(a)',end=99,err=99) line
            if(line(1:1)=='#'.or.line(1:1)=='!') goto 19
            call parse(line,tokens,ntok)
            if(tokens(1)=='#'.or.tokens(1)=='!') goto 19

            if(iver==1) then
               if(ntok.ne.5.and.(tokens(6).ne.'#'.and.  &
                                 tokens(6).ne.'!')) then
                  print*,'iver:',iver
                  print*,'ntok:',ntok
                  print*,'incorrect number of tokens'
                  stop 'plotspc: grabber initialization'
               endif

               ! cannot do this with mem alloc until f90
               !read(tokens(1),'(a)') idsta
               read(tokens(1),*) iidsta
               read(tokens(2),*) flat
               read(tokens(3),*) flon
               read(tokens(4),*) elev
               read(tokens(5),*) itype
               write(*,'(i10,3f11.5,i3)') iidsta,flat,flon,elev,itype
            endif

            call ll_xy (flat,flon,platn(ngd),plonn(ngd),xfx,yfy)
            CALL findgrid (xfx,yfy,zfz,ngridg,xmn,nxpmax,nnxp,ymn  &
                          ,nypmax,nnyp,zmn,nzpmax,nnzp,ngdb,ngde)
            if(nint(flat)==-999.or.nint(flon)==-999.or.ngridg==-1) then
               if(nint(flat)==-999.or.nint(flon)==-999)  &
                  print*,'lat or lon = -999.'
               goto 19
            endif

            if(itype==1) then
               numpts=numpts+1
            elseif(itype==2) then
               if(action(1:5)=='STATS') numpts=numpts+nplevs+1
               if(action(1:4)=='GRAB') numpts=numpts+nnzp(ngridg)-1
            else
              print*,'  illegal type',itype,' -> must be = 1 (pt) or 2 (prof)'
              goto 19
            endif
            goto 19
            99 continue
            close(31)
            
            ! allocate grabber and stats memory
            if(allocated(fpos)) deallocate (fpos)
            if(allocated(fobs)) deallocate (fobs)
            if(allocated(pred)) deallocate (pred)
            numtime=1+nint((itend-itbeg)/float(itstep))
            allocate (fpos(numtime*numpts*8))
            allocate (fobs(numtime*numpts*numfore))
            allocate (pred(numtime*numpts*numfore))
         endif

         if(action(1:5)=='STATS') then
            call STAT_init (fobs,pred,fpos,ngdb,ngde)
         elseif(action(1:4)=='GRAB') then
            call GRAB_init (pred,fpos,ngdb,ngde)
         endif
      endif

      do nngd=ngdb,ngde
         print*
         write(*,'(a,i3)') 'Doing grid - ',nngd

         ! for GRAB or STATS, if no pts on grid, skip
         if((action(1:5)=='STATS'.or.action(1:4)=='GRAB').and.  &
            igrabgrd(nngd)==0) goto 10

         write(cgrid,'(i1)') nngd
         write(ctime,'(i3)') nfile

         ! loop through max number of plots
         ivvar=0
         do nplot=1,maxfore

            igrabgrd(nngd)=0

            ! only do this if have readable primary var
            if(cframe_a(nplot)(1:1).ne.'/') goto 210
            
            ! want to retain the previous values
            if(nplot.ne.1) then
               cdname1(nplot)=cdname1(nplotcur)
               cdunits1(nplot)=cdunits1(nplotcur)
            endif
            nplotcur=nplot
            
            frtokens(1)=' '
            call tokenize1 (cframe_a(nplot),frtokens,ntokfr,toksepfr)
            if(action(1:5).ne.'SPACE'.and.  &
               frtokens(1)=='') frtokens(1)='none'
            if(action(1:5).ne.'SPACE'.and.  &
               frtokens(1)=='none') goto 210
            ivvar=ivvar+1

            cvar(1)=frtokens(1)
            
            ! prohibit use of interpollated wind directions
            if(frtokens(1)(1:9)=='direction'.and.  &
               action(1:5).ne.'SPACE'.and.action(1:4).ne.'DUMP') then
               print*,'Cannot output wind direction at interpollated grid points'
               stop 'plotspc'
            endif

            if(action(1:5)=='SPACE') then
            
               ! set defaults
               call continit ()
            
               ! get CFRAME_A attributes
               call contset (1,'CFRAME_A',cframe_a(nplot))
               
               ! get CFRAME_B attributes
               frtokens(1)=' '
               call tokenize1 (cframe_b(nplot),frtokens,ntokfr,toksepfr)
               if(len_trim(frtokens(1)).ge.1) then
                  cwinds=frtokens(1)(1:1)
                  call windset (cframe_b(nplot))
               endif
               
               ! get CFRAME_C attributes
               frtokens(1)=' '
               call tokenize1 (cframe_c(nplot),frtokens,ntokfr,toksepfr)
               if(len_trim(frtokens(1)).ge.1) then
                  cvar(2)=frtokens(1)
                  call contset (2,'CFRAME_C',cframe_c(nplot))
               endif
               
               ! get landmark attributes
               call landmkset ()
               
               ! adjust colors
               call custcolors ()
               
            endif
            
            ! print action summary
            print*
            if(action(1:5)=='SPACE') then
               if(cvar(1)(1:4).ne.'none') write(*,'(4a)')  &
                   'Plotting prim var - '  &
                  ,cvar(1)(1:len_trim(cvar(1))),' / ',conttyp(1)
                  
               if(cwinds=='b') write(*,'(a)')  &
                   'Plotting winds    - barbs'
               if(cwinds=='v') write(*,'(a)')  &
                   'Plotting winds    - vectors'
               if(cwinds=='s') write(*,'(a)')  &
                   'Plotting winds    - streamlines'
               if(cwinds=='t') write(*,'(a)')  &
                   'Plotting winds    - vectors and turbulence'
               if(cwinds=='r') write(*,'(a)')  &
                   'Plotting winds    - vectors and vorticity'
            
               if(cvar(2)(1:4).ne.'none') write(*,'(4a)')  &
                   'Plotting sec var  - '  &
                  ,cvar(2)(1:len_trim(cvar(2))),' / ',conttyp(2)
                  
            else
               if(cvar(1)(1:4).ne.'none')  &
                  print*,'Doing var- ',cvar(1)(1:len(cvar(1)))
            endif

            ! set up the background configuration from input strings
            call backset(nplot)
 
            if(horiz=='X'.and.vert=='Z') then
               islab=1
               niboff=ixbeg
               nieoff=ixend
               niinc=ixstep
               njboff=izbeg
               njeoff=izend
               njinc=izstep
               nnboff=iybeg
               nneoff=iyend
               nninc=iystep
               icoor=iybeg
               if(iybeg.le.0) icoor=1-iybeg
            elseif(horiz=='Y'.and.vert=='Z') then
               islab=2
               niboff=iybeg
               nieoff=iyend
               niinc=iystep
               njboff=izbeg
               njeoff=izend
               njinc=izstep
               nnboff=ixbeg
               nneoff=ixend
               nninc=ixstep
               icoor=ixbeg
               if(ixbeg.le.0) icoor=1-ixbeg
            elseif(horiz=='X'.and.vert=='Y') then
               islab=3
               niboff=ixbeg
               nieoff=ixend
               niinc=ixstep
               njboff=iybeg
               njeoff=iyend
               njinc=iystep
               nnboff=izbeg
               nneoff=izend
               nninc=izstep
               icoor=izbeg
               if(iztran(ivvar)==3) call get_pcoor(icoor,iplevel(ivvar))
            elseif(horiz=='Y'.and.vert=='X') then
               print*,'XVAR must be on horizontal axis'
               stop 'plotspc'
               
            elseif(horiz=='Z') then
               print*,'ZVAR must be on vertical axis'
               stop 'plotspc'
               
            endif
            if(icoor.le.0) icoor=1-icoor
            !print*,'islab,icoor',islab,icoor
            !print*,'niboff,nieoff,nninc',niboff,nieoff,nninc
            !print*,'njboff,njeoff,njinc',njboff,njeoff,njinc
            !print*,'nnboff,nneoff,niinc',nnboff,nneoff,niinc
            
            ! prevent cartesian and pressure plots in the vert
            if(islab.ne.3.and.iztran(nplot).ne.1) then
               print*,'nplot',nplot
               print*,'iztran(nplot)',iztran(nplot)
               print*,'islab',islab
               print*,'Cannot plot vertical cartesian or pressure surfaces'
               stop 'plotspc'
            endif
            
            ! prohibit use of interpollated wind directions
            if((cvar(1)(1:9)=='direction'.or.  &
               cvar(2)(1:9)=='direction').and.  &
               iztran(nplot).ne.1) then
               print*,'Cannot output wind direction at interpollated grid points'
               stop 'plotspc'
            endif

            ! ensure slab=3 for V5D GRADS, GRIB, MEDOC or DUMP
            if((action(1:3)=='V5D'  .or.action(1:4)=='DUMP'.or.  &
                action(1:5)=='GRADS'.or.action(1:4)=='GRIB'.or.  &
                action(1:5)=='MEDOC')  &
                .and.islab.ne.3) then
               print*,'Must output a horizontal slab for '  &
                     ,'Vis5d, GrADS, GRIB, MEDOC or Dump output'
               stop 'plotspc'
            endif
   
            ! set variables which are not passed by revu,
            ! but are required by read_RAMS
            ccont='y'
            ppx1=0.
            ppx2=0.
            ppy1=0.
            ppy2=0.

            call read_RAMS ('revu',action,ivvar,ivtime  &
                           ,cgrid,ctime,iztran(nplot),ccont  &
                           ,niboff,nieoff,njboff,njeoff,nnboff,nneoff  &
                           ,head1,ppx1,ppx2,ppy1,ppy2,fobs,pred,fpos  &
                           ,cframe_a,maxfore,igunit,glats,glatn,glonw,glone  &
                           ,irefg,jrefg,xref,yref,dategrads,cincgrads  &
                           ,igridll,glldllat,glldllon  &
                           ,gllwlon,gllelon,gllslat,gllnlat  &
                           ,cdname1(nplot),cdunits1(nplot),revpref)

            if(ivtime==1.and.ivvar==1.and.  &
               action(1:5)=='GRADS') dategrads1=dategrads

            if(action(1:4).ne.'DUMP'.and.  &
               action(1:3).ne.'V5D'.and.  &
               action(1:4).ne.'GRAB'.and.  &
               action(1:5).ne.'GRADS'.and.  &
               action(1:4).ne.'GRIB'.and.  &
               action(1:5).ne.'MEDOC'.and.  &
               mod(ivvar,ipanl)==0) call frame

            if(action(1:5)=='GRADS')then
              gradsvar(ivvar)=cvar(1)
              izztran=iztran(nplot)
            endif

            210 continue

         enddo
         10 continue
      enddo
      12 continue
   enddo

   if(action(1:3)=='V5D') then
   
      n=v5dclose()
      if(n==0) then
         print*,'value returned:',n
         stop 'RAMS_v5dh- v5dClose'
      endif
      print*
      print*,'Vis5d file closed'
      print*,'---------------------------------'
      
   elseif(action(1:5)=='GRADS') then
   
      call RAMS_get_fdata(2,1,ngd,startutc,nval)
      call RAMS_get_fdata(0,1,ngd,ztn(1,ngd),nval)
      call RAMS_gradsbin(igunit,'CLOSE',izztran,ngd,ndum,ndum  &
                        ,iyear1,imonth1,idate1,itime1,revpref)
      print*,'GrADS binary file closed: '
      call RAMS_gradsctl(igunit,ngd,platn(ngd),plonn(ngd)  &
                        ,deltaxn(ngd),deltayn(ngd)  &
                        ,glats,glatn,glonw,glone,irefg,jrefg  &
                        ,xref,yref,izztran,dategrads1,cincgrads  &
                        ,nib,nie,njb,nje,nnb,nne,izstep  &
                        ,ztn(1,ngd),ivtime,ivvar,gradsvar,cdname1  &
                        ,cdunits1,iyear1,imonth1,idate1,itime1,revpref)
      print*,'GrADS control file closed: '
      print*,'---------------------------------'
      
   elseif(action(1:4)=='GRIB') then
   
      close(igunit)
      print*
      print*,'GRIB file closed: '
      print*,'---------------------------------'
      
   endif
   
enddo

if(action(1:5).eq.'SPACE') then
   ! print summary of max and min values
   print*
   print*,'min-max for each level, plot and grid'
   do il=1,maxlayers
      do iv=1,ivvar
         do nngd=ngdb,ngde
            if(len_trim(cdnames(il,iv,nngd)).ne.0) then
               write(*,'(3i3,2e12.4,5a)') il,iv,nngd   &
                  ,clomin(il,iv,nngd),chimax(il,iv,nngd),'  '  &
                  ,cdnames(il,iv,nngd)(1:len_trim(cdnames(il,iv,nngd))-1),' ['  &
                  ,cdunitss(il,iv,nngd)(1:len_trim(cdunitss(il,iv,nngd))-1),']'
            endif
         enddo
      enddo
   enddo
endif

if(action(1:4).ne.'DUMP'.and.action(1:3).ne.'V5D'.and.  &
   action(1:4).ne.'GRAB'.and.action(1:5).ne.'GRADS'.and.  &
   action(1:4).ne.'GRIB'.and.action(1:5).ne.'MEDOC'.and.  &
   ipanl.gt.1.and.mod(ivvar,ipanl)==0) then
   call frame
endif

! once all loops are complete and all data 'grabbed', send
! control to output (GRAB) or stats computation (STATS)

if(action(1:4)=='GRAB') then
  
   call RAMS_get_cdata (0,1,flnm1,nval)
   if(igrabfmt==1) then
   
      write(flnm,'(2a,a2,i4.4,a1,i2.2,a1,i2.2,a1,i4.4,a9)' )  &
          revpref(1:len_trim(revpref))  &
         ,flnm1(lastslash(flnm1)+1:len_trim(flnm1)-27),'S-'  &
         ,iyear1,'-',imonth1,'-',idate1,'-',itime1,'00-g0.gbr'
      print*
      print*,'Grabber file: ',flnm(1:len_trim(flnm))
   
      ! some file
      open(unit=33,file=flnm,status='unknown')
      write(33,'(a1,a9,2a10,6a15,a10,5x,30a15)')  &
         '!','date','time','stat id','lat','lon','elev'  &
        ,'x','y','z','grd',(stchar(i1),i1=1,numfore)
      nfndx=0
      do nfile=ibegs,iends,itstep
         nfndx=nfndx+1
         call GRAB_output (33,pred,fpos)
      enddo
      close(33)

   elseif(igrabfmt==2) then
      
      ! stuff to do... 
      !   don't like this file name
      !   put surface obs into dp-s and profile obs into dp-r
      !   maybe don't have ralph_in file
      !   need to interpollate height for each location
      !   text name and integer fields (instead of all real)
   
      write(flnm,'(a,a5,a,a2,i4.4,a1,i2.2,a1,i2.2,a1,i4.4)' )  &
          revpref(1:len_trim(revpref))  &
         ,'dp-s-',flnm1(lastslash(flnm1)+1:len_trim(flnm1)-27)  &
         ,'S-',iyear1,'-',imonth1,'-',idate1,'-',itime1
      print*
      print*,'RALPH2 file: ',flnm(1:len_trim(flnm))

      ! ralph surface file (can't have anything AGL)
      open(33,file=flnm,status='unknown',form='formatted')
      rewind 33

      ! write header information
      imarker=999999
      iver_sfc=2
      write(33,'(i6,1x,i1)') imarker,iver_sfc
      write(33,'(a2)')'5'
      write(33,'(a14)')'WINDSPEED  m/s'
      write(33,'(a19)')'WIND_DIRECTION  deg'
      write(33,'(a14)')'TEMPERATURE  C'
      write(33,'(a11)')'DEWPOINT  K'
      write(33,'(a12)')'STN_PRES  Pa'

      ! write data
      nfndx=0
      do nfile=ibegs,iends,itstep
         nfndx=nfndx+1
         call write_ralph (33,pred,fpos)
      enddo
      close(33)
   else
      stop 'unknown grabber format specified'
   endif
endif

if(action(1:5)=='STATS') then
   nfndx=0
   do nfile=ibegs,iends,itstep
      nfndx=nfndx+1
      call RAMS_stats (fobs,pred,fpos)
      call STATS_output (fobs,pred,fpos)
   enddo
endif

return
end

!***************************************************************************

subroutine backset (n)

implicit none

integer :: n

include 'vcomm2.h' 

horiz=' '
vert=' '
linv=' '
linh=' '
fix1=' '
fix2=' '
fix3=' '
call var_parse (xvar(n),'X',ixbeg,ixend,ixstep,horiz,vert,fix1,fix2,fix3)
call var_parse (yvar(n),'Y',iybeg,iyend,iystep,horiz,vert,fix1,fix2,fix3)
call var_parse (zvar(n),'Z',izbeg,izend,izstep,horiz,vert,fix1,fix2,fix3)
call var_parse (tvar(n),'T',itbeg,itend,itstep,horiz,vert,fix1,fix2,fix3)
ixstep=max(1,ixstep)
iystep=max(1,iystep)
izstep=max(1,izstep)

if(horiz.ne.' '.and.vert==' ') then
   linh=horiz
elseif(horiz==' '.and.vert.ne.' ') then
   linv=vert
endif

return
end

!***************************************************************************

subroutine var_parse (string,vchar,isbeg,isend,istep,h,v,f1,f2,f3)

! This routine takes the strings in XVAR,YVAR,ZVAR and TVAR and
! parses them out into more usable components which are used by the
! rest of the program

character*(*) string,h,v,f1,f2,f3,vchar
character fmt*5
character tokens(50)*8,toksep(3)*1
data toksep/'/',':',','/

call tokenize (string,tokens,ntok,toksep,3)

! Start parsing the range information, which is delimeted
! by colons and must start at the fourth token

call ch2int(tokens(4),isbeg)
if(ntok.gt.6.and.number(tokens(6))==1) then
   call ch2int (tokens(6),isend)
else
   isend=isbeg
endif

if(ntok.gt.8.and.number(tokens(8))==1) then
   call ch2int (tokens(8),istep)
else
   istep=1
endif

! This variable is the abscissa or ordinate
if (tokens(2)=='H') h=vchar
if (tokens(2)=='V') v=vchar

! This variable is fixed
if(tokens(2)=='F'.and.f1==' ') then
   f1=vchar
elseif(tokens(2)=='F'.and.f2==' ') then
   f2=vchar
elseif(tokens(2)=='F') then
   f3=vchar
endif

return
end
