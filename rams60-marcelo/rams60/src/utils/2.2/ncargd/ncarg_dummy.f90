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
! 2.2.0
!###########################################################################

subroutine ncarg_dummies
character*(*) c1,c2,c3,c4
external ext1

entry arinam(i1,i2)
entry arscam(i1,f1,f2,i2,i3,i4,i5,ext1)
entry cfux(f1)
entry cfuy(f1)
entry chkcyc(f1,f2,i1,i2,i3)
entry clgen(f1,i1,i2,i3,f2,f3,f4,i4,i5,f5,i6,i7)
entry clsgks()
entry conrec(f1,i1,i2,i3,f2,f3,f4,i4,i5,i6)
entry cpclam(f1,f2,i1,i2)
entry cpcldr(f1,f2,i1)
entry cpcnrc(f1,f2,f3,i1,i2,i3,f4,f5,f6,i4,i5,i6)
entry cpezct(f1,i1,i2)
entry cpgeti(c1,i1)
entry cpgetr(c1,f1)
entry cplbdr(f1,f2,i1)
entry cppkcl(f1,f2,i1)
entry cprect(f1,i1,i2,i3,f2,i4,i5,i6)
entry cpsetc(c1,c2)
entry cpseti(c1,i1)
entry cpsetr(c1,f1)
entry curve(f1,f2,i1)
entry curved(f1,f2,i1)
entry dashdb(i1)
entry dashdc(c1,i1,i2)
entry drwstr(f1,f2,f3,f4,i1,i2)
entry encd(f1,f2,c1,i1,i2)
entry ezcntr(f1,i1,i2)
entry frame()
entry frstpt(f1,f2)
entry gacwk(i1)
entry gclks(i1)
entry gclrwk(i1)
entry gclwk(i1)
entry gdawk(i1)
entry gessdc(i1,f1,f2)
entry gesspn(c1)
entry getset(f1,f2,f3,f4,f5,f6,f7,f8,i1)
entry getusv(c1,i1)
entry gfa(i1,f1,f2)
entry gflas1(i1)
entry gflas2()
entry gflas3(i1)
entry gopks(i1,i2)
entry gopwk(i1,i2,i3)
entry gpl(i1,f1,f2)
entry gpm(i1,f1,f2)
entry gqasf(i1,i2)
entry gqcntn(i1,i2)
entry gqcr(i1,i2,i3,i4,r1,r2,r3)
entry gqnt(i1,i2,f1,f2)
entry gqplci(i1,i2)
entry gqtxci(i1,i2)
entry gsasf(i1)
entry gsclip(i1)
entry gscr(i1,i2,f1,f2,f3)
entry gselnt(i1)
entry gsfaci(i1)
entry gsfais(i1)
entry gslwsc(f1)
entry gsmk(i1)
entry gsmksc(f1)
entry gsplci(i1)
entry gspmci(i1)
entry gstxci(i1)
entry hlsrgb(r1,r2,r3,r4,r5,r6)
entry hsvrgb(f1,f2,f3,f4,f5,f6)
entry i1mach(i1)
entry ishift(c1,i1)
entry lblbar(i1,f1,f2,f3,f4,i2,f5,f6,i3,i4,c1,i5,i6)
entry lbseti(c1,i1)
entry line(f1,f2,f3,f4)
entry lined(f1,f2,f3,f4)
entry mapaci(i1)
entry mapbla(i1)
entry mapdrw()
entry mapint()
entry mapiq()
entry mapiqa(i1,i2,i3,i4)
entry mapit(f1,f2,i1)
entry mapita(f1,f2,i1,i2,i3,i4,i5)
entry mappos(f1,f2,f3,f4)
entry maproj(c1,f1,f2,f3)
entry maprs()
entry mapset(c1,f1,f2,f3,f4)
entry mapstc(c1,c2)
entry mapsti(c1,i1)
entry maptra(f1,f2,f3,f4)
entry maptri(f1,f2,f3,f4)
entry maptrn(f1,f2,f3,f4)
entry minmax(f1,i1,i2,i3,i4,f2,i5)
entry mpsetc(c1,c2)
entry niceinc(f1,f2,i1,f3,f4)
entry niceinc6(f1,f2,i1,f3,f4)
entry opngks()
entry pcgeti(c1,i1)
entry pcgetr(c1,f1)
entry pcseti(c1,i1)
entry perim(i1,i2,i3,i4)
entry plchhq(f1,f2,c1,f3,f4,f5)
entry plchmq(f1,f2,c1,f3,f4,f5)
entry plchlq(f1,f2,c1,f3,f4,f5)
entry plotif(f1,f2,i1)
entry plotit(i1,i2,i3)
entry point(f1,f2)
entry points(f1,f2,i1,i2,i3)
entry pwritx(f1,f2,c1,i1,i2,i3,f5)
entry q8qst4(c1,c2,c3,c4)
entry reord(f1,i1,f2,i2,i3)
entry reset()
entry rgbhls(r1,r2,r3,r4,r5,r6)
entry set(f1,f2,f3,f4,f5,f6,f7,f8,i1)
entry seter(c1,i1,i2)
entry setusv(c1,i1)
entry sflush()
entry sfseti(c1,i1)
entry sfsgfa(f1,f2,i1,f3,i2,i3,i4,i5)
entry stline(f1,i1,i2,i3,f2)
entry strmln(f1,f2,f3,i1,i2,i3,i4,i5)
entry vector(f1,f2)
entry velvct(f1,i1,f2,i2,i3,i4,f3,f4,i5,i6,i7,f5)
entry wtstr(f1,f2,c1,i1,i2,i3)

return
end

!##############################################################

subroutine gsetmarkercolourind(f1)
return
end


