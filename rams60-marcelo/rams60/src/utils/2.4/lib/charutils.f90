!
! Copyright (C) 1991-2005  ; All Rights Reserved ; ATMET, LLC
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

integer function lastchar(str)
implicit none
character(len=*) :: str
integer :: n,ln
! returns last non-blank character position from a string

ln=len(str)
do n=ln,1,-1
   if(str(n:n).ne.' ') then
      lastchar=n
      return
   endif
enddo
lastchar=0

return
end

!***************************************************************************

integer function ifirstchar(str)
implicit none
character(len=*) :: str
integer :: n,ln

! returns first non-blank character position from a string

ln=len(str)
do n=1,ln
   if(str(n:n).ne.' ') then
      ifirstchar=n
      return
   endif
enddo
ifirstchar=1

return
end

!***************************************************************************

subroutine string_replace(str1,str2,ncbeg,ncend)
implicit none
character(len=*) :: str1,str2
character(len=1024) :: str3
integer :: n2,nch,ncbeg,ncend

! replace string2 in string1(ncbeg:ncend). removes trailing blanks from str2. 

nch=ncbeg
if(ncbeg > 1) str3=str1(1:ncbeg-1)

str3(nch:)=trim(str2)
n2=len_trim(trim(str2))
str3(ncbeg+n2:)=str1(ncend+1:)

str1 = str3

return
end

!***************************************************************************

subroutine deblank(str1,str2,nch)
implicit none
character(len=*) :: str1,str2
integer :: n,ln,nch

! strips blanks from a string and returns number of chars

str2=' '
ln=len(str1)
nch=0
do n=1,ln
   if(str1(n:n).ne.' ') then
      nch=nch+1
      str2(nch:nch)=str1(n:n)
   endif
enddo

return
end

!***************************************************************************

subroutine detab(str1,str2,nch)
implicit none
character(len=*) :: str1,str2
integer :: n,ln,nch
character(len=1) ::  tab

tab=achar( 9)

! strips tabs from a string and returns number of chars

str2=' '
ln=len_trim(str1)
nch=0
do n=1,ln
   if(str1(n:n).ne.tab) then
      !print*,'no tab:',str1(n:n)
      nch=nch+1
      str2(nch:nch)=str1(n:n)
   else
      print*,'found one:',str1
      str2(nch+1:nch+6)='      '
      nch=nch+6
   endif
enddo

return
end

!***************************************************************************

integer function lastslash(str)
implicit none
character(len=*) :: str
integer :: n,ln

! returns last slash character position from a string

ln=len(str)
do n=ln,1,-1
   if(str(n:n).eq.'/') then
      lastslash=n
      return
   endif
enddo
lastslash=0

return
end

!***************************************************************************

subroutine char_strip_var(line,var,line2)
implicit none
character(len=*) :: line,var,line2
integer :: nn,ncl,nb

! removes instances of a substring from a string

ncl=len(line)
do nn=1,ncl
   if(line(nn:nn).ne.' ') then
      nb=index(line(nn:),' ')
      var=line(nn:nn+nb-1)
      goto 25
   endif
enddo
25 continue
line2=line(nn+nb-1:)

return
end

!***************************************************************************

subroutine findln(text,ltext,order)
implicit none
character(len=*) :: text
integer :: ltext,order
integer :: i

! find first non-blank character if order=0, last non-blank if order=1

if(order.eq.1) then
   do i=len(text),1,-1
      if(text(i:i).ne.' ') then
         ltext=i
         goto 10
      endif
   enddo
   10 continue
else
   do i=1,len(text)
      if(text(i:i).ne.' ') then
         ltext=i
         goto 20
      endif
   enddo
   20 continue
endif

return
end

!***************************************************************************

subroutine parse(str,tokens,ntok)
implicit none
integer :: ntok
character(len=*) :: str,tokens(*)
character(len=1) :: sep
integer, parameter :: ntokmax=100

integer :: n,nc,npt,nch,ntbeg,ntend

! this routine "parses" character string str into different pieces
! or tokens by looking for  possible token separators (toks
! str contains nch characters.  the number of tokens identified is nto
! the character string tokens are stored in tokens.

sep=' '
ntok=0
npt=1
nch=len_trim(str)
nc=1
do ntok=1,ntokmax
   do n=nc,nch
      if(str(n:n) /= sep) then
         ntbeg=n
         exit
      endif
   enddo
   
   do n=ntbeg,nch
      if(str(n:n) == sep .or. str(n:n) == char(10)) then ! look for \n also
         ntend=n-1
         goto 22
      endif
      if(n == nch) then
         ntend=n
         exit
      endif
   enddo
   22 continue
   tokens(ntok)=str(ntbeg:ntend)
   nc=ntend+1
   if(nc >= nch) goto 25
enddo

25 continue

!do nc=1,nch
!   if(str(nc:nc).eq.sep.or.nc.eq.nch)then
!      if(nc-npt.ge.1)then
!         ntok=ntok+1
!         tokens(ntok)=str(npt:nc-1)
!         if(nc.eq.nch.and.str(nc:nc).ne.sep)then
!            tokens(ntok)=str(npt:nc)
!            go to 10
!         endif
!      endif
!      ntok=ntok+1
!      tokens(ntok)=str(nc:nc)
!      npt=nc+1
!      go to 10
!   endif
!   10 continue
!enddo

return
end

!***************************************************************************

subroutine tokenize(str1,tokens,ntok,toksep,nsep)
implicit none
integer :: nsep,ntok
character(len=*) :: str1,tokens(*)
character(len=1) :: toksep(nsep)

character(len=256) :: str
integer :: npt,nch,nc,ns

! this routine "parses" character string str into different pieces
! or tokens by looking for  possible token separators (toks
! str contains nch characters.  the number of tokens identified is nto
! the character string tokens are stored in tokens.

ntok=0
npt=1
call deblank(str1,str,nch)
do nc=1,nch
   do ns=1,nsep
      if(str(nc:nc).eq.toksep(ns).or.nc.eq.nch) then
         if(nc-npt.ge.1)then
            ntok=ntok+1
            tokens(ntok)=str(npt:nc-1)
            if(nc.eq.nch.and.str(nc:nc).ne.toksep(ns)) then
               tokens(ntok)=str(npt:nc)
               goto 10
            endif
         endif
         ntok=ntok+1
         tokens(ntok)=str(nc:nc)
         npt=nc+1
         goto 10
      endif
   enddo
10      continue
enddo
return
end

!***************************************************************************

subroutine tokenize1(str1,tokens,ntok,toksep)
implicit none
integer :: ntok
character(len=*) :: str1,tokens(*)
character(len=1) :: toksep

character(len=256) :: str
integer :: nch,ist,npt,nc

! this routine "parses" character string str into different pieces
! or tokens by looking for  possible token separators (toks
! str contains nch characters.  the number of tokens identified is nto
! the character string tokens are stored in tokens.

call deblank(str1,str,nch)

ist=1
if(str(1:1) == toksep) ist=2
npt=ist
ntok=0
do nc=ist,nch
         !print*,'++:',trim(str)
         !print*,'ttttttttttt0:',ist,nch,nc,npt
         !print*,'ttttttttttt0:',str(nc:nc),toksep
   if(str(nc:nc) == toksep .or. nc == nch) then
      if(nc-npt >= 0) then
         ntok=ntok+1
         tokens(ntok)=str(npt:nc-1)
         !print*,'ttttttttttt1--:',str(npt:nc-1)
         !print*,'ttttttttttt1:',ntok,npt,nc,tokens(ntok)
         if(nc == nch .and. str(nc:nc) /= toksep) then
            tokens(ntok)=str(npt:nc)
            !print*,'ttttttttttt2:',ntok,npt,nc,tokens(ntok)
            exit
         endif
         npt=nc+1
      elseif(nc == nch) then
         ntok=ntok+1
         tokens(ntok)=str(npt:nc)
         !print*,'ttttttttttt3:',ntok,npt,nc,tokens(ntok)
         exit
      endif
   endif
enddo

return
end

!***************************************************************************

subroutine tokenize2(str,tokens,ntok,toksep)
implicit none
integer :: ntok
character(len=*) :: str,tokens(*)
character(len=1) :: toksep

integer :: nch,ist,npt,nc

! same as tokenize1, but doesn't deblank the string

str = adjustl(str)
nch = len_trim(str)

ist=1
if(str(1:1) == toksep) ist=2
npt=ist
ntok=0
do nc=ist,nch
         !print*,'++:',trim(str)
         !print*,'ttttttttttt0:',ist,nch,nc,npt
         !print*,'ttttttttttt0:',str(nc:nc),toksep
   if(str(nc:nc) == toksep .or. nc == nch) then
      if(nc-npt >= 0) then
         ntok=ntok+1
         tokens(ntok)=str(npt:nc-1)
         !print*,'ttttttttttt1--:',str(npt:nc-1)
         !print*,'ttttttttttt1:',ntok,npt,nc,tokens(ntok)
         if(nc == nch .and. str(nc:nc) /= toksep) then
            tokens(ntok)=str(npt:nc)
            !print*,'ttttttttttt2:',ntok,npt,nc,tokens(ntok)
            exit
         endif
         npt=nc+1
      elseif(nc == nch) then
         ntok=ntok+1
         tokens(ntok)=str(npt:nc)
         !print*,'ttttttttttt3:',ntok,npt,nc,tokens(ntok)
         exit
      endif
   endif
enddo

return
end

!***************************************************************************

subroutine tokfind(toks,ntok,str,iff)
implicit none
integer :: ntok,iff
character(len=*) :: toks(*),str

integer :: n

! looks for a number of tokens (substrings) within a string

do n=1,ntok
   !print*,'tokfind-',n,toks(n)(1:lastchar(toks(n)))  &
   !      ,'=====',str(1:lastchar(str))
   if(trim(str) == trim(toks(n))) then
      iff=1
      return
   endif
enddo
iff=0

return
end

!***************************************************************************

subroutine rams_intsort(ni,nums,cstr)
implicit none
integer :: nums(*),ni
character(len=*) :: cstr(*)

character(len=200) :: cscr
integer :: n,mini,nm,nmm,nscr

! sort an array of character strings by an associated integer field

do n=1,ni
   mini=1000000
   do nm=n,ni
      if(nums(nm).lt.mini) then
         nmm=nm
         mini=nums(nm)
      endif
   enddo
   nscr=nums(n)
   nums(n)=nums(nmm)
   nums(nmm)=nscr
   cscr=cstr(n)
   cstr(n)=cstr(nmm)
   cstr(nmm)=cscr
enddo

return
end

!***************************************************************************

subroutine rams_fltsort(ni,xnums,cstr)
implicit none
integer :: ni
real :: xnums(*)
character(len=*) :: cstr(*)

character(len=200) :: cscr
integer :: n,nm,nmm
real :: xmini,xnscr

! sort an array of character strings by an associated float field

do n=1,ni
   xmini=1.e30
   do nm=n,ni
      if(xnums(nm).lt.xmini) then
         nmm=nm
         xmini=xnums(nm)
      endif
   enddo
   xnscr=xnums(n)
   xnums(n)=xnums(nmm)
   xnums(nmm)=xnscr
   cscr=cstr(n)
   cstr(n)=cstr(nmm)
   cstr(nmm)=cscr
enddo

return
end

!***************************************************************************

logical function isnumber (str)
implicit none
character(len=*) :: str

integer :: nc, ifirstchar

! First character number check - test to see if all characters of
! the string STR are numeric:  ISNUMBER = .f. if 'no', = .t. if 'yes' 
! (including a decimal point or minus sign or e). Leading or trailing blanks
!  are ignored.

isnumber=.true.

do nc = ifirstchar(str),len_trim(str)
   if( (str(nc:nc) < '0'.or.str(nc:nc) > '9' ) .and. &
        str(nc:nc) /= '.' .and.  &
        str(nc:nc) /= '+' .and. str(nc:nc) /= '-' .and. &
        str(nc:nc) /= 'e' .and. str(nc:nc) /= 'E') isnumber=.false.
enddo

return
end

!***************************************************************************

logical function is_integer (str)
implicit none
character(len=*) :: str

integer :: nc, ifirstchar, nstart

! Positive integer check - test to see if all characters of
! the string STR are numeric:  pos_integer = .f. if 'no', = .t. if 'yes'.
! Leading or trailing blanks are ignored.

is_integer=.true.

nstart=ifirstchar(str)
if(str(nstart:nstart) == '-') nstart=nstart+1

do nc = ifirstchar(str),len_trim(str)
   if( str(nc:nc) < '0' .or. str(nc:nc) > '9' ) is_integer=.false.
enddo

return
end

!***************************************************************************

integer function letter (str)
implicit none
character*(*) str

! First character alpha check - test to see if the first character of
! the string STR is alphabetic: LETTER = 0 if 'no', = 1 if 'yes'.

letter=0
if((str(1:1).ge.'A'.and.str(1:1).le.'Z').or.  &
   (str(1:1).ge.'a'.and.str(1:1).le.'z')) letter=1
   
return
end

!***************************************************************************

integer function number (str)
implicit none
character(len=*) :: str

! First character number check - test to see if the first character of
! the string STR is numeric:  NUMBER = 0 if 'no', = 1 if 'yes' (includ
! a decimal point or minus sign).

number=0
if(str(1:1).ge.'0'.and.str(1:1).le.'9') number=1
if(str(1:1).eq.'.'.or.str(1:1).eq.'-') number=1

return
end

!***************************************************************************

integer function letint (str)
implicit none
character(len=*) :: str

! First character integer variable check - test to see if the first
! character of STR is an I, J, K, L, M, or N, or i, j, k, l ,m or n:
! LETINT = 0 if 'no', = 1 if 'yes'

letint=0
if((str(1:1).ge.'I'.and.str(1:1).le.'N').or.  &
   (str(1:1).ge.'i'.and.str(1:1).le.'n')) letint=1
   
return
end

!***************************************************************************

integer function letquo (str)
implicit none
character(len=*) :: str

! First character quote check - test to see if the first character
! of STR is a quote:  LETQUO = 0 if 'no', = 1 if 'yes'.

letquo=0
if(str(1:1).eq.'''') letquo=1

return
end
